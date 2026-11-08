package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.InternalArg2pApi
import it.unibo.tuprolog.argumentation.engine.term.Arg2pUnificator
import it.unibo.tuprolog.argumentation.engine.term.collectVariables
import it.unibo.tuprolog.argumentation.engine.term.renamedCopy
import it.unibo.tuprolog.argumentation.engine.words.IdSet
import it.unibo.tuprolog.argumentation.engine.words.WordTable
import it.unibo.tuprolog.argumentation.engine.words.Words
import it.unibo.tuprolog.argumentation.engine.words.Words.ATOM
import it.unibo.tuprolog.argumentation.engine.words.Words.FUN
import it.unibo.tuprolog.argumentation.engine.words.Words.LIST
import it.unibo.tuprolog.argumentation.engine.words.Words.REF
import it.unibo.tuprolog.argumentation.engine.words.Words.STR
import it.unibo.tuprolog.argumentation.engine.words.payload
import it.unibo.tuprolog.argumentation.engine.words.tag
import it.unibo.tuprolog.argumentation.engine.words.word
import it.unibo.tuprolog.argumentation.engine.words.wordKey
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Truth
import it.unibo.tuprolog.core.Var

/**
 * Clause database backing the argumentation context: records (the words of the machine, off its heap, see
 * [Record]), only ever asserted (asserta order), retracted and looked up, in the encoding of one program (the
 * machine's once [bind] is called, a private one until then).
 *
 * A lookup reads the goal where it is, on the heap of the machine or in a record ([candidates]): each argument position
 * of a predicate is indexed, when first looked up by, by the top symbol of the stored arguments and, for large buckets,
 * by a hash of the ground ones, and so is each place inside its clauses declared for it ([declared]). What it gives are
 * candidates, which the unification of their heads decides.
 *
 * Branches share their data until one of them is modified (copy-on-write), and then only copy the predicates they
 * modify. A lookup sees the clauses as they are when it is made (logical update view): appending writes past the end
 * a lookup holds, and removing makes a new array. The term API (the methods on terms) serves the solvers that are not
 * the machine, and the derived data.
 */
@InternalArg2pApi
class ClauseStore private constructor(
    private var data: Data,
    private var shared: Boolean,
    encoding: Program?,
    // The places the predicates (`functor/arity`) are looked up by, besides their arguments
    private val declared: Map<String, List<Place>>,
) {
    constructor(
        encoding: Program? = null,
        declared: Map<String, List<Place>> = emptyMap(),
    ) : this(Data(declared), false, encoding, declared)

    /** A place inside the clauses of a predicate: an argument of the head and the way from it through list cells, each step 0 for the head of a cell, 1 for its tail */
    class Place(
        val argument: Int,
        val steps: IntArray,
    )

    /** The program whose words the records are in */
    var encoding: Program? = encoding
        private set

    private fun encoding(): Program = encoding ?: Program().also { encoding = it }

    /** Whether no clause is stored */
    fun isEmpty(): Boolean = data.isEmpty()

    /** The records re-encoded in the words of [program] (the machine's): kept as they are if they are its words already */
    fun bind(program: Program) {
        if (encoding === program) return
        val compatible = program.readsWordsOf(encoding)
        encoding = program
        if (data.isEmpty() || compatible) return
        val stored = data.all()
        data = Data(declared)
        shared = false
        for (record in stored) data.add(Record.encoded(record, program))
    }

    fun fork(): ClauseStore {
        shared = true
        return ClauseStore(data, true, encoding, declared)
    }

    private fun writable(): Data {
        if (shared) {
            data = data.copy()
            shared = false
        }
        return data
    }

    /** asserta: the new record is found before the existing ones */
    fun assertA(record: Record) {
        val encoded = Record.encoded(record, encoding())
        val head = encoded.head
        // (a clause must have a callable head: reading it as a clause raises the error)
        if (tag(head) == REF || !isCallable(head)) encoded.clause
        writable().add(encoded)
    }

    private fun isCallable(head: Int) = tag(head) == STR || tag(head) == LIST || tag(head) == ATOM

    fun assertA(term: Term) = assertA(Record.of(term))

    /** [stored], candidates of this store just looked up, removed */
    fun removeAll(stored: List<Stored>) {
        if (stored.isNotEmpty()) writable().removeAll(stored)
    }

    /** The stored records whose head may unify with [head] (a record of the head alone), newest first */
    fun candidates(head: Record): List<Stored> {
        val encoded = Record.encoded(head, encoding())
        return candidates(encoded.words, encoded.root).toList()
    }

    /** The stored records whose head may unify with the term [goal] (dereferenced) of [words]: the heap of the machine or a record */
    fun candidates(
        words: IntArray,
        goal: Int,
    ): Candidates = data.candidates(encoding(), words, goal)

    /**
     * A value derived from the records of the predicate [key] (`functor/arity`, newest first), named [name]: computed
     * once, until the predicate changes (the branches sharing the predicate share it)
     */
    fun <T : Any> derived(
        key: String,
        name: String,
        compute: (List<Stored>) -> T,
    ): T {
        val encoding = encoding()
        if (derivedOf !== encoding) {
            derivedOf = encoding
            derivedKeys.clear()
        }
        val functor =
            derivedKeys.getOrPut(key) {
                val slash = key.lastIndexOf('/')
                val arity = key.substring(slash + 1).toInt()
                val name0 = key.substring(0, slash)
                if (arity == 0) encoding.atomWord(name0) else encoding.functorWord(name0, arity)
            }
        return data.derived(functor, name, compute)
    }

    // The words of the keys of [derived], for the program they are of
    private var derivedOf: Program? = null
    private val derivedKeys = HashMap<String, Int>()

    /** The integers of the facts `name(Integer)`, newest first, each once (the set is not to be changed) */
    fun ids(name: String): IdSet = data.ids(encoding().functorWord(name, 1))

    /**
     * The integers of the facts `name(Id)` from the [from]-th asserted on (0 for all), in the order they were asserted,
     * and how many there are: the facts asserted since a count taken before, if none was retracted
     */
    fun idsFrom(
        name: String,
        from: Int,
    ): Pair<IntArray, Int> = data.idsFrom(encoding().functorWord(name, 1), from)

    /** The facts of `name/arity` from the [from]-th asserted on, in the order they were asserted, and how many there are */
    fun recordsFrom(
        name: String,
        arity: Int,
        from: Int,
    ): Pair<List<Record>, Int> {
        val stored = data.stored(encoding().functorWord(name, arity))
        return List(maxOf(0, stored.size - from)) { stored[from + it].record } to stored.size
    }

    /** How many clauses of `name/arity` are stored */
    fun count(
        name: String,
        arity: Int,
    ): Int = data.count(encoding().functorWord(name, arity))

    /** The facts stored as [record], up to the names of their variables, retracted: whether there were any */
    fun retractFact(record: Record): Boolean {
        val encoded = Record.encoded(record, encoding())
        val key = encoded.key
        val candidates = candidates(encoded.words, encoded.root)
        val found = ArrayList<Stored>()
        for (i in 0 until candidates.size) {
            val stored = candidates.items[i]!!
            if (!stored.record.isRule && stored.record.key == key) found += stored
        }
        removeAll(found)
        return found.isNotEmpty()
    }

    /** Whether the fact [record] is stored, up to the names of its variables */
    fun holds(record: Record): Boolean {
        val encoded = Record.encoded(record, encoding())
        val key = encoded.key
        val candidates = candidates(encoded.words, encoded.root)
        for (i in 0 until candidates.size) {
            val stored = candidates.items[i]!!.record
            if (!stored.isRule && stored.key == key) return true
        }
        return false
    }

    /** The fact `name(id)` added */
    fun assertId(
        name: String,
        id: Int,
    ) {
        val program = encoding()
        assertA(Record(program, intArrayOf(program.functorWord(name, 1), program.intWord(id.toLong())), word(STR, 0), null))
    }

    /** The fact `name(A)` added, for [fact], the fact `other(A)`, its words shared: what A is, is not read */
    fun assertNamed(
        name: String,
        fact: Record,
    ) {
        val program = encoding()
        assertA(Record.encoded(fact, program).named(program.functorWord(name, 1)))
    }

    /** The facts `name(A)` [assertNamed] added for [facts], removed (the others keep their order) */
    fun retractNamed(
        name: String,
        facts: List<Record>,
    ) {
        if (facts.isEmpty()) return
        val program = encoding()
        val wanted = facts.mapTo(HashSet(facts.size * 2)) { Record.encoded(it, program).key }
        removeAll(data.stored(program.functorWord(name, 1)).filter { it.record.key in wanted })
    }

    /** The facts `name(id)` [assertId] added for [ids], removed (the others keep their order) */
    fun retractIds(
        name: String,
        ids: IdSet,
    ) {
        if (ids.size == 0) return
        removeAll(
            data.stored(encoding().functorWord(name, 1)).filter { stored ->
                stored.record.idAt(0) in ids
            },
        )
    }

    // ---- the term API

    /** Solutions of a goal (a fact, a single-goal rule, or `clause(Head, Body)`) on a snapshot of the store */
    fun solve(goal: Term): Sequence<Substitution> {
        val struct = goal.asStruct() ?: return emptySequence()
        val variables = HashSet<Var>().also { struct.collectVariables(it) }
        val solutions =
            if (struct.functor == "clause" && struct.arity == 2) {
                lookupClause(struct[0], struct[1])
            } else {
                resolve(struct, Substitution.empty(), 0)
            }
        return solutions.map { unifier -> unifier.filter { v, _ -> v in variables } }
    }

    private fun lookupClause(
        head: Term,
        body: Term,
    ): Sequence<Substitution> {
        val headStruct = head.asStruct() ?: return emptySequence()
        val query = Struct.of("clause", headStruct, body)
        return candidates(Record.of(headStruct)).asSequence().mapNotNull { stored ->
            val fresh = stored.clause.renamedCopy()
            Arg2pUnificator
                .mgu(query, Struct.of("clause", fresh.head!!, fresh.body))
                .takeIf { it.isSuccess }
        }
    }

    private fun resolve(
        goal: Term,
        unifier: Substitution,
        depth: Int,
    ): Sequence<Substitution> {
        val current = goal.apply(unifier)
        if (current is Truth && current.isTrue) return sequenceOf(unifier)
        val struct = current.asStruct() ?: return emptySequence()
        if (struct.functor == "," && struct.arity == 2) {
            return resolve(struct[0], unifier, depth).flatMap { resolve(struct[1], it, depth) }
        }
        check(depth < MAX_DEPTH) { "Context rules nested too deeply while solving $goal" }
        return candidates(Record.of(struct)).asSequence().flatMap { stored ->
            val fresh = stored.clause.renamedCopy()
            val step = Arg2pUnificator.mgu(struct, fresh.head!!)
            if (!step.isSuccess) {
                emptySequence()
            } else {
                val composed = (unifier + step).takeIf { it.isSuccess } ?: return@flatMap emptySequence<Substitution>()
                resolve(fresh.body, composed, depth + 1)
            }
        }
    }

    /** A record stored, with its identity in the store */
    class Stored(
        val id: Int,
        val record: Record,
    ) {
        val clause: Clause get() = record.clause
    }

    /** Stored clauses a lookup is to try: the first [size] of [items], the newest last, as they are when it is made */
    class Candidates(
        val items: Array<Stored?>,
        val size: Int,
    ) {
        /** Newest first */
        fun toList(): List<Stored> = (size - 1 downTo 0).map { items[it]!! }

        companion object {
            val NONE = Candidates(arrayOfNulls(0), 0)
        }
    }

    // Clauses in the order they were asserted. An append past the end of what a lookup holds does not disturb it; a
    // removal makes a new array. An array two predicates share (a copy of one) is copied by the first that appends
    private class Bucket(
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

        fun copy(): Bucket {
            sharedItems = true
            return Bucket(items, size, true)
        }

        fun candidates(): Candidates = if (size == 0) Candidates.NONE else Candidates(items, size)
    }

    // The index of an argument position or of a place, built when first looked up by: by the top symbol of the stored
    // terms there, and for the ground ones by a hash of the whole of them (that one built when a bucket turns out to be large)
    private class Position(
        val place: Place,
    ) {
        private fun termOf(record: Record): Int = placed(record.words, argumentOf(record, place.argument), place.steps)

        val byKey = WordTable<Bucket>()
        val variables = Bucket()
        var groundByHash: HashMap<Int, Bucket>? = null
        var nonGround: Bucket? = null

        fun add(stored: Stored) {
            val argument = termOf(stored.record)
            val key = wordKey(stored.record.words, argument)
            if (key == 0) variables.append(stored) else byKey.getOrPut(key) { Bucket() }.append(stored)
            val ground = groundByHash ?: return
            val hash = hashOf(stored.record.program!!, stored.record.words, argument)
            if (hash == NOT_GROUND) nonGround!!.append(stored) else ground.getOrPut(hash) { Bucket() }.append(stored)
        }

        // The buckets holding [stored] without them ([ids], ascending, are their identifiers)
        fun removeAll(
            stored: List<Stored>,
            ids: IntArray,
        ) {
            val affected = HashSet<Bucket>()
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

        fun buildGround(all: Bucket) {
            if (groundByHash != null) return
            val ground = HashMap<Int, Bucket>()
            val rest = Bucket()
            for (i in 0 until all.size) {
                val stored = all.items[i]!!
                val hash = hashOf(stored.record.program!!, stored.record.words, termOf(stored.record))
                if (hash == NOT_GROUND) rest.append(stored) else ground.getOrPut(hash) { Bucket() }.append(stored)
            }
            groundByHash = ground
            nonGround = rest
        }
    }

    // The clauses by a hash of their whole head, the ground ones: what a goal with all its arguments bound goes through,
    // when none of them alone narrows the clauses down
    private class Heads {
        val ground = HashMap<Int, Bucket>()
        val nonGround = Bucket()

        private fun bucketOf(stored: Stored): Bucket? {
            val record = stored.record
            val hash = groundHash(record.program!!, record.words, record.head)
            return if (hash == NOT_GROUND) nonGround else ground[hash]
        }

        fun add(stored: Stored) {
            val record = stored.record
            val hash = groundHash(record.program!!, record.words, record.head)
            (if (hash == NOT_GROUND) nonGround else ground.getOrPut(hash) { Bucket() }).append(stored)
        }

        fun removeAll(
            stored: List<Stored>,
            ids: IntArray,
        ) {
            val affected = HashSet<Bucket>()
            for (one in stored) bucketOf(one)?.let { affected += it }
            for (bucket in affected) bucket.without(ids)
        }
    }

    private class Predicate(
        val arity: Int,
        // The data that can change it: the other data holding it have it from a copy, and make their own before a change
        val owner: Data,
        // The arguments, then the places declared for it
        val places: Array<Place>,
    ) {
        var all = Bucket()
        var positions = arrayOfNulls<Position>(places.size)
        private var builtHeads: Heads? = null

        fun heads(): Heads =
            builtHeads ?: Heads().also { heads ->
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

        fun copy(newOwner: Data): Predicate = Predicate(arity, newOwner, places).also { it.all = all.copy() }

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

        fun position(index: Int): Position =
            positions[index] ?: Position(places[index]).also { position ->
                for (i in 0 until all.size) position.add(all.items[i]!!)
                positions[index] = position
            }
    }

    private class Data(
        private val declared: Map<String, List<Place>>,
        private val predicates: WordTable<Predicate> = WordTable(),
        private var nextId: Int = 0,
    ) {
        fun copy(): Data = Data(declared, predicates.copy(), nextId)

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
        ): Predicate {
            val predicate = predicates[key]
            if (predicate?.owner === this) return predicate
            return (predicate?.copy(this) ?: newPredicate(key, record)).also { predicates[key] = it }
        }

        private fun newPredicate(
            key: Int,
            record: Record,
        ): Predicate {
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
            return Predicate(arity, this, places)
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
            var best: Bucket? = all
            var other: Bucket? = null
            // (the places declared first, the narrowest; candidates this few are not worth hashing the rest of the goal)
            for (i in predicate.places.indices.reversed()) {
                if (bestSize <= SCANNED) break
                val place = predicate.places[i]
                val argument = placed(words, derefIn(words, goalArgument(words, goal, place.argument)), place.steps)
                val key = wordKey(words, argument)
                if (key == 0) continue
                val position = predicate.position(i)
                var bucket = position.byKey[key]
                var rest: Bucket? = position.variables
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
            a: Bucket,
            b: Bucket,
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

    companion object {
        private val NO_STEPS = IntArray(0)

        // A variable for what no cell leads to: a term with another shape has no key there
        private const val NOWHERE = 0

        // At most this many clauses are tried in turn, with no index
        private const val SCANNED = 4

        // The candidates by top symbol from which a ground argument is hashed as a whole
        private const val GROUNDED_FROM = 8
        private const val MAX_DEPTH = 64
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
    }
}
