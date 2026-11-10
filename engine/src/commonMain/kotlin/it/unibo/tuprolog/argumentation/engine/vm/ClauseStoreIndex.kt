package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.vm.ClauseStore.Candidates
import it.unibo.tuprolog.argumentation.engine.vm.ClauseStore.Place
import it.unibo.tuprolog.argumentation.engine.vm.ClauseStore.Stored
import it.unibo.tuprolog.argumentation.engine.words.IdSet
import it.unibo.tuprolog.argumentation.engine.words.WordTable
import it.unibo.tuprolog.argumentation.engine.words.Words
import it.unibo.tuprolog.argumentation.engine.words.Words.FUN
import it.unibo.tuprolog.argumentation.engine.words.Words.LIST
import it.unibo.tuprolog.argumentation.engine.words.Words.REF
import it.unibo.tuprolog.argumentation.engine.words.Words.STR
import it.unibo.tuprolog.argumentation.engine.words.payload
import it.unibo.tuprolog.argumentation.engine.words.tag
import it.unibo.tuprolog.argumentation.engine.words.word
import it.unibo.tuprolog.argumentation.engine.words.wordKey

// The index of a ClauseStore: its predicates, the clauses of each by argument position and by place inside them, and the
// data a store shares with its branches until it modifies it (see ClauseStore)

// Clauses in the order they were asserted. An append past the end of what a lookup holds does not disturb it; a
// removal makes a new array. An array two predicates share (a copy of one) is copied by the first that appends
internal class StoreBucket(
    var items: Array<Stored?> = arrayOfNulls(4),
    var size: Int = 0,
    var sharedItems: Boolean = false,
) {
    fun append(stored: Stored) {
        if (sharedItems || size == items.size) {
            items = items.copyOf(maxOf(4, size * 2))
            sharedItems = false
        }
        items[size++] = stored
    }

    // (the items of a bucket, as [removed], are by ascending identifiers)
    fun without(removed: IntArray) {
        val kept = arrayOfNulls<Stored>(maxOf(4, size))
        var n = 0
        var from = 0
        for (i in 0 until size) {
            val stored = items[i]!!
            // (found by halves: many buckets, each with little of what is removed, are not to walk it all)
            var low = from
            var high = removed.size
            while (low < high) {
                val middle = (low + high) ushr 1
                if (removed[middle] < stored.id) low = middle + 1 else high = middle
            }
            from = low
            if (from == removed.size || removed[from] != stored.id) kept[n++] = stored
        }
        items = kept
        size = n
        sharedItems = false
    }

    fun copy(): StoreBucket {
        sharedItems = true
        return StoreBucket(items, size, true)
    }

    fun candidates(): Candidates = if (size == 0) Candidates.NONE else Candidates(items, size)
}

// The index of an argument position or of a place, built when first looked up by: by the top symbol of the stored
// terms there, and for the ground ones by a hash of the whole of them (that one built when a bucket turns out to be large)
internal class StorePosition(
    val place: Place,
) {
    private fun termOf(record: Record): Int = placed(record.words, argumentOf(record, place.argument), place.steps)

    val byKey = WordTable<StoreBucket>()
    val variables = StoreBucket()
    var groundByHash: HashMap<Int, StoreBucket>? = null
    var nonGround: StoreBucket? = null

    fun add(stored: Stored) {
        val argument = termOf(stored.record)
        val key = wordKey(stored.record.words, argument)
        if (key == 0) variables.append(stored) else byKey.getOrPut(key) { StoreBucket() }.append(stored)
        val ground = groundByHash ?: return
        val hash = hashOf(stored.record.program!!, stored.record.words, argument)
        if (hash == NOT_GROUND) nonGround!!.append(stored) else ground.getOrPut(hash) { StoreBucket() }.append(stored)
    }

    // The buckets holding [stored] without them ([ids], ascending, are their identifiers)
    fun removeAll(
        stored: List<Stored>,
        ids: IntArray,
    ) {
        val affected = HashSet<StoreBucket>()
        for (one in stored) {
            val argument = termOf(one.record)
            val key = wordKey(one.record.words, argument)
            (if (key == 0) variables else byKey[key])?.let { affected += it }
            val ground = groundByHash ?: continue
            val hash = hashOf(one.record.program!!, one.record.words, argument)
            (if (hash == NOT_GROUND) nonGround else ground[hash])?.let { affected += it }
        }
        for (bucket in affected) bucket.without(ids)
    }

    fun buildGround(all: StoreBucket) {
        if (groundByHash != null) return
        val ground = HashMap<Int, StoreBucket>()
        val rest = StoreBucket()
        for (i in 0 until all.size) {
            val stored = all.items[i]!!
            val hash = hashOf(stored.record.program!!, stored.record.words, termOf(stored.record))
            if (hash == NOT_GROUND) rest.append(stored) else ground.getOrPut(hash) { StoreBucket() }.append(stored)
        }
        groundByHash = ground
        nonGround = rest
    }
}

// The clauses by a hash of their whole head, the ground ones: what a goal with all its arguments bound goes through,
// when none of them alone narrows the clauses down
internal class StoreHeads {
    val ground = HashMap<Int, StoreBucket>()
    val nonGround = StoreBucket()

    private fun bucketOf(stored: Stored): StoreBucket? {
        val record = stored.record
        val hash = groundHash(record.program!!, record.words, record.head)
        return if (hash == NOT_GROUND) nonGround else ground[hash]
    }

    fun add(stored: Stored) {
        val record = stored.record
        val hash = groundHash(record.program!!, record.words, record.head)
        (if (hash == NOT_GROUND) nonGround else ground.getOrPut(hash) { StoreBucket() }).append(stored)
    }

    fun removeAll(
        stored: List<Stored>,
        ids: IntArray,
    ) {
        val affected = HashSet<StoreBucket>()
        for (one in stored) bucketOf(one)?.let { affected += it }
        for (bucket in affected) bucket.without(ids)
    }
}

internal class StorePredicate(
    val arity: Int,
    // The data that can change it: the other data holding it have it from a copy, and make their own before a change
    val owner: StoreData,
    // The arguments, then the places declared for it
    val places: Array<Place>,
) {
    var all = StoreBucket()
    var positions = arrayOfNulls<StorePosition>(places.size)
    private var builtHeads: StoreHeads? = null

    fun heads(): StoreHeads =
        builtHeads ?: StoreHeads().also { heads ->
            for (i in 0 until all.size) heads.add(all.items[i]!!)
            builtHeads = heads
        }

    // The values derived from the clauses (see ClauseStore.derived), until they change
    val derived = HashMap<String, Any>()

    // The integers of the first arguments, for a predicate of identifiers (see ClauseStore.ids), until they change
    var ids: IdSet? = null

    fun readIds(): IdSet {
        val found = IdSet(all.size)
        for (i in all.size - 1 downTo 0) {
            val id = all.items[i]!!.record.idAt(0)
            if (id >= 0) found.add(id)
        }
        return found
    }

    fun copy(newOwner: StoreData): StorePredicate = StorePredicate(arity, newOwner, places).also { it.all = all.copy() }

    fun add(stored: Stored) {
        derived.clear()
        ids = null
        all.append(stored)
        for (position in positions) position?.add(stored)
        builtHeads?.add(stored)
    }

    fun removeAll(stored: List<Stored>) {
        derived.clear()
        ids = null
        val removed = IntArray(stored.size) { stored[it].id }
        removed.sort()
        // (most of the clauses gone: looked up again, the index costs no more than taking them out one by one)
        val rebuild = stored.size * 4 > all.size
        all.without(removed)
        if (rebuild) {
            positions = arrayOfNulls(places.size)
            builtHeads = null
        } else {
            for (position in positions) position?.removeAll(stored, removed)
            builtHeads?.removeAll(stored, removed)
        }
    }

    fun position(index: Int): StorePosition =
        positions[index] ?: StorePosition(places[index]).also { position ->
            for (i in 0 until all.size) position.add(all.items[i]!!)
            positions[index] = position
        }
}

internal class StoreData(
    private val declared: Map<String, List<Place>>,
    private val predicates: WordTable<StorePredicate> = WordTable(),
    private var nextId: Int = 0,
) {
    fun copy(): StoreData = StoreData(declared, predicates.copy(), nextId)

    fun isEmpty(): Boolean = predicates.values().all { it.all.size == 0 }

    /** Every record, oldest first */
    fun all(): List<Record> =
        predicates
            .values()
            .flatMap { predicate -> (0 until predicate.all.size).map { predicate.all.items[it]!! } }
            .sortedBy { it.id }
            .map { it.record }

    /** The predicate stored under key, copied the first time this data modifies it */
    private fun writable(
        key: Int,
        record: Record,
    ): StorePredicate {
        val predicate = predicates[key]
        if (predicate?.owner === this) return predicate
        return (predicate?.copy(this) ?: newPredicate(key, record)).also { predicates[key] = it }
    }

    private fun newPredicate(
        key: Int,
        record: Record,
    ): StorePredicate {
        val arity = arityOf(record, record.head)
        val declared =
            if (tag(key) ==
                FUN
            ) {
                declared["${record.program!!.nameOfFunctorWord(key)}/$arity"].orEmpty()
            } else {
                emptyList()
            }
        val places = Array(arity + declared.size) { if (it < arity) Place(it, NO_STEPS) else declared[it - arity] }
        return StorePredicate(arity, this, places)
    }

    fun add(record: Record) {
        writable(keyOf(record), record).add(Stored(nextId++, record))
    }

    fun removeAll(stored: List<Stored>) {
        for ((key, group) in stored.groupBy { keyOf(it.record) }) {
            writable(key, group.first().record).removeAll(group)
        }
    }

    fun <T : Any> derived(
        key: Int,
        name: String,
        compute: (List<Stored>) -> T,
    ): T {
        val predicate = predicates[key] ?: return compute(emptyList())
        @Suppress("UNCHECKED_CAST")
        return predicate.derived.getOrPut(name) {
            val all = predicate.all
            compute((all.size - 1 downTo 0).map { all.items[it]!! })
        } as T
    }

    fun ids(key: Int): IdSet {
        val predicate = predicates[key] ?: return IdSet.NONE
        return predicate.ids ?: predicate.readIds().also { predicate.ids = it }
    }

    fun idsFrom(
        key: Int,
        from: Int,
    ): Pair<IntArray, Int> {
        val all = predicates[key]?.all ?: return IntArray(0) to 0
        val found = ArrayList<Int>()
        for (i in from until all.size) {
            val id = all.items[i]!!.record.idAt(0)
            if (id >= 0) found += id
        }
        return found.toIntArray() to all.size
    }

    fun count(key: Int): Int = predicates[key]?.all?.size ?: 0

    /** The records of the predicate [key], oldest first */
    fun stored(key: Int): List<Stored> {
        val all = predicates[key]?.all ?: return emptyList()
        return (0 until all.size).map { all.items[it]!! }
    }

    fun candidates(
        program: Program,
        words: IntArray,
        goal: Int,
    ): Candidates {
        val predicate = predicates[wordKey(words, goal)] ?: return Candidates.NONE
        val all = predicate.all
        if (predicate.arity == 0 || all.size <= SCANNED) return all.candidates()
        var bestSize = all.size
        var best: StoreBucket? = all
        var other: StoreBucket? = null
        // (the places declared first, the narrowest; candidates this few are not worth hashing the rest of the goal)
        for (i in predicate.places.indices.reversed()) {
            if (bestSize <= SCANNED) break
            val place = predicate.places[i]
            val argument = placed(words, derefIn(words, goalArgument(words, goal, place.argument)), place.steps)
            val key = wordKey(words, argument)
            if (key == 0) continue
            val position = predicate.position(i)
            var bucket = position.byKey[key]
            var rest: StoreBucket? = position.variables
            var size = (bucket?.size ?: 0) + rest!!.size
            if (size == 0) return Candidates.NONE
            if (size > GROUNDED_FROM && (tag(argument) == STR || tag(argument) == LIST)) {
                val hash = groundHash(program, words, argument)
                if (hash != NOT_GROUND) {
                    position.buildGround(all)
                    val ground = position.groundByHash!![hash]
                    val open = position.nonGround
                    val narrowed = (ground?.size ?: 0) + open!!.size
                    if (narrowed == 0) return Candidates.NONE
                    if (narrowed < size) {
                        bucket = ground
                        rest = open
                        size = narrowed
                    }
                }
            }
            if (size < bestSize) {
                bestSize = size
                best = bucket
                other = rest
            }
        }
        if (bestSize > GROUNDED_FROM && predicate.arity > 1) {
            val hash = groundHash(program, words, goal)
            if (hash != NOT_GROUND) {
                val heads = predicate.heads()
                best = heads.ground[hash]
                other = heads.nonGround
            }
        }
        return when {
            other == null || other.size == 0 -> best?.candidates() ?: Candidates.NONE
            best == null || best.size == 0 -> other.candidates()
            else -> merged(best, other)
        }
    }

    // Two buckets as one, in the order of their clauses
    private fun merged(
        a: StoreBucket,
        b: StoreBucket,
    ): Candidates {
        val n = a.size + b.size
        val items = arrayOfNulls<Stored>(n)
        var i = 0
        var j = 0
        for (k in 0 until n) {
            items[k] =
                if (j == b.size || (i < a.size && a.items[i]!!.id < b.items[j]!!.id)) a.items[i++] else b.items[j++]
        }
        return Candidates(items, n)
    }
}

private val NO_STEPS = IntArray(0)

// A variable for what no cell leads to: a term with another shape has no key there
private const val NOWHERE = 0

// At most this many clauses are tried in turn, with no index
private const val SCANNED = 4

// The candidates by top symbol from which a ground argument is hashed as a whole
private const val GROUNDED_FROM = 8

private const val NOT_GROUND = Int.MIN_VALUE

private fun derefIn(
    words: IntArray,
    w0: Int,
): Int {
    var w = w0
    while (tag(w) == REF) {
        val c = words[payload(w)]
        if (c == w) return w
        w = c
    }
    return w
}

// The key of the predicate of a record
private fun keyOf(record: Record): Int = if (record.functor != 0) record.functor else wordKey(record.words, record.head)

private fun arityOf(
    record: Record,
    head: Int,
): Int =
    when (tag(head)) {
        STR -> record.program!!.arity(record.words[payload(head)])
        LIST -> 2
        else -> 0
    }

// The term at the end of [steps] from the dereferenced term [from] of [words], NOWHERE if they lead to no term
private fun placed(
    words: IntArray,
    from: Int,
    steps: IntArray,
): Int {
    var w = from
    for (step in steps) {
        if (tag(w) != LIST) return NOWHERE
        w = derefIn(words, words[payload(w) + step])
    }
    return w
}

private fun hashOf(
    program: Program,
    words: IntArray,
    w: Int,
): Int = if (tag(w) == REF) NOT_GROUND else groundHash(program, words, w)

private fun goalArgument(
    words: IntArray,
    goal: Int,
    i: Int,
): Int = if (tag(goal) == LIST) words[payload(goal) + i] else words[payload(goal) + 1 + i]

private fun argumentOf(
    record: Record,
    i: Int,
): Int = derefIn(record.words, goalArgument(record.words, record.head, i))

private fun mix(
    hash: Int,
    word: Int,
): Int {
    val mixed = (hash xor word) * -1640531527
    // (never the mark of what is not ground: it would only look so, which costs but a longer search)
    return (mixed xor (mixed ushr 15)).let { if (it == NOT_GROUND) 1 else it }
}

// A hash of the term [w0] of [words], the same wherever it is laid out; NOT_GROUND with a variable in it
private fun groundHash(
    program: Program,
    words: IntArray,
    w0: Int,
): Int = hashInto(program, words, w0, 1125899)

private fun hashInto(
    program: Program,
    words: IntArray,
    w0: Int,
    seed: Int,
): Int {
    var hash = seed
    var w = derefIn(words, w0)
    while (true) {
        when (tag(w)) {
            REF -> return NOT_GROUND
            STR -> {
                val a = payload(w)
                val f = words[a]
                hash = mix(hash, f)
                val n = program.arity(f)
                for (i in 1 until n) {
                    hash = hashInto(program, words, words[a + i], hash)
                    if (hash == NOT_GROUND) return NOT_GROUND
                }
                w = derefIn(words, words[a + n])
            }
            LIST -> {
                val a = payload(w)
                hash = hashInto(program, words, words[a], mix(hash, Words.LIST_KEY))
                if (hash == NOT_GROUND) return NOT_GROUND
                w = derefIn(words, words[a + 1])
            }
            else -> return mix(hash, w)
        }
    }
}
