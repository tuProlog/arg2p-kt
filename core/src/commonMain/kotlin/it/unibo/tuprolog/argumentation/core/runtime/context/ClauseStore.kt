package it.unibo.tuprolog.argumentation.core.runtime.context

import it.unibo.tuprolog.argumentation.core.engine.Record
import it.unibo.tuprolog.argumentation.core.engine.vm.Program
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.ATOM
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.INT
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.LIST
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.REF
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.STR
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.payload
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.tag
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.word
import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.argumentation.core.runtime.collectVariables
import it.unibo.tuprolog.argumentation.core.runtime.renamedCopy
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
 * by a hash of the ground ones. What it gives are candidates, which the unification of their heads decides.
 *
 * Branches share their data until one of them is modified (copy-on-write), and then only copy the predicates they
 * modify. A lookup sees the clauses as they are when it is made (logical update view): appending writes past the end
 * a lookup holds, and removing makes a new array. The term API (the methods on terms) serves the solvers that are not
 * the machine, and the derived data.
 */
internal class ClauseStore private constructor(
    private var data: Data,
    private var shared: Boolean,
    encoding: Program?,
) {
    constructor(encoding: Program? = null) : this(Data(), false, encoding)

    /** The program whose words the records are in */
    var encoding: Program? = encoding
        private set

    private fun encoding(): Program = encoding ?: Program().also { encoding = it }

    /** Whether no clause is stored */
    fun isEmpty(): Boolean = data.isEmpty()

    /** The records re-encoded in the words of [program] (the machine's) */
    fun bind(program: Program) {
        if (encoding === program) return
        encoding = program
        if (data.isEmpty()) return
        val stored = data.all()
        data = Data()
        shared = false
        for (record in stored) data.add(Record.encoded(record, program))
    }

    fun fork(): ClauseStore {
        shared = true
        return ClauseStore(data, true, encoding)
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
        val slash = key.lastIndexOf('/')
        val arity = key.substring(slash + 1).toInt()
        val name0 = key.substring(0, slash)
        val encoding = encoding()
        val functor = if (arity == 0) encoding.atomWord(name0) else encoding.functorWord(name0, arity)
        return data.derived(functor, name, compute)
    }

    /** The integers of the facts `name(Integer)`, newest first, each once (the set is not to be changed) */
    fun ids(name: String): IdSet = data.ids(encoding().functorWord(name, 1))

    /** The fact `name(id)` added */
    fun assertId(
        name: String,
        id: Int,
    ) {
        val program = encoding()
        assertA(Record(program, intArrayOf(program.functorWord(name, 1), program.intWord(id.toLong())), word(STR, 0), null))
    }

    /** The fact `name(A)` added, for the stored clause [clause], `Head :- body(A)`: what A is, is not read */
    fun assertArgumentOfBody(
        name: String,
        clause: Stored,
    ) {
        val program = encoding()
        val record = clause.record
        val argument = record.words[payload(record.body) + 1]
        val n = record.words.size
        val words = record.words.copyOf(n + 2)
        words[n] = program.functorWord(name, 1)
        words[n + 1] = argument
        assertA(Record(program, words, word(STR, n), null))
    }

    // ---- the term API

    /** retractall: removes every clause whose head (and body, for a `:-` pattern) unifies with the given term */
    fun retractAll(term: Term) {
        val pattern = term.asStruct() ?: return
        val (head, body) = if (pattern.isRule()) pattern[0] to pattern[1] else pattern to null
        val headStruct = head.asStruct() ?: return
        removeAll(
            candidates(Record.of(headStruct)).filter { stored ->
                val fresh = stored.clause.renamedCopy()
                val unifier = Arg2pUnificator.mgu(headStruct, fresh.head!!)
                unifier.isSuccess && (body == null || Arg2pUnificator.mgu(body.apply(unifier), fresh.body.apply(unifier)).isSuccess)
            },
        )
    }

    /** The stored clauses whose head may unify with the given one, newest first, as stored */
    fun clausesFor(head: Term): List<Clause> =
        head.asStruct()?.let { struct -> candidates(Record.of(struct)).map { it.clause } } ?: emptyList()

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

    /**
     * Bodies of the clauses whose head unifies with the given one, as stored: the head bindings are not applied,
     * so variables in the stored clauses are left untouched
     */
    fun unifiableBodies(
        head: Term,
        body: Term,
    ): Sequence<Substitution> {
        val headStruct = head.asStruct() ?: return emptySequence()
        val variables = HashSet<Var>().also { body.collectVariables(it) }
        return candidates(Record.of(headStruct)).asSequence().mapNotNull { stored ->
            val fresh = stored.clause.renamedCopy()
            if (!Arg2pUnificator.mgu(headStruct, fresh.head!!).isSuccess) return@mapNotNull null
            Arg2pUnificator
                .mgu(body, fresh.body)
                .takeIf { it.isSuccess }
                ?.filter { v, _ -> v in variables }
        }
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
        val id: Long,
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
        fun without(removed: LongArray) {
            val kept = arrayOfNulls<Stored>(maxOf(4, size))
            var n = 0
            var next = 0
            for (i in 0 until size) {
                val stored = items[i]!!
                while (next < removed.size && removed[next] < stored.id) next++
                if (next == removed.size || removed[next] != stored.id) kept[n++] = stored
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

    // The index of an argument position, built when first looked up by: by the top symbol of the stored arguments, and
    // for the ground ones by a hash of the whole of them (that one built when a bucket turns out to be large)
    private class Position(
        val index: Int,
    ) {
        val byKey = WordTable<Bucket>()
        val variables = Bucket()
        var groundByHash: HashMap<Long, Bucket>? = null
        var nonGround: Bucket? = null

        fun add(stored: Stored) {
            val argument = argumentOf(stored.record, index)
            val key = topKey(stored.record.words, argument)
            if (key == 0) variables.append(stored) else byKey.getOrPut(key) { Bucket() }.append(stored)
            val ground = groundByHash ?: return
            val hash = groundHash(stored.record.program!!, stored.record.words, argument)
            if (hash == NOT_GROUND) nonGround!!.append(stored) else ground.getOrPut(hash) { Bucket() }.append(stored)
        }

        // The buckets holding [stored] without them ([ids], ascending, are their identifiers)
        fun removeAll(
            stored: List<Stored>,
            ids: LongArray,
        ) {
            val affected = HashSet<Bucket>()
            for (one in stored) {
                val argument = argumentOf(one.record, index)
                val key = topKey(one.record.words, argument)
                (if (key == 0) variables else byKey[key])?.let { affected += it }
                val ground = groundByHash ?: continue
                val hash = groundHash(one.record.program!!, one.record.words, argument)
                (if (hash == NOT_GROUND) nonGround else ground[hash])?.let { affected += it }
            }
            for (bucket in affected) bucket.without(ids)
        }

        fun buildGround(all: Bucket) {
            if (groundByHash != null) return
            val ground = HashMap<Long, Bucket>()
            val rest = Bucket()
            for (i in 0 until all.size) {
                val stored = all.items[i]!!
                val hash = groundHash(stored.record.program!!, stored.record.words, argumentOf(stored.record, index))
                if (hash == NOT_GROUND) rest.append(stored) else ground.getOrPut(hash) { Bucket() }.append(stored)
            }
            groundByHash = ground
            nonGround = rest
        }
    }

    private class Predicate(
        val arity: Int,
        // The data that can change it: the other data holding it have it from a copy, and make their own before a change
        val owner: Data,
    ) {
        var all = Bucket()
        var positions = arrayOfNulls<Position>(arity)

        // The values derived from the clauses (see ClauseStore.derived), until they change
        val derived = HashMap<String, Any>()

        // The integers of the first arguments, for a predicate of identifiers (see ClauseStore.ids), until they change
        var ids: IdSet? = null

        fun readIds(): IdSet {
            val found = IdSet(all.size)
            for (i in all.size - 1 downTo 0) {
                val record = all.items[i]!!.record
                val word = record.words[payload(record.head) + 1]
                if (tag(word) == INT) found.add(payload(word))
            }
            return found
        }

        fun copy(newOwner: Data): Predicate = Predicate(arity, newOwner).also { it.all = all.copy() }

        fun add(stored: Stored) {
            derived.clear()
            ids = null
            all.append(stored)
            for (position in positions) position?.add(stored)
        }

        fun removeAll(stored: List<Stored>) {
            derived.clear()
            ids = null
            val removed = LongArray(stored.size) { stored[it].id }
            removed.sort()
            // (most of the clauses gone: looked up again, the index costs no more than taking them out one by one)
            val rebuild = stored.size * 4 > all.size
            all.without(removed)
            if (rebuild) {
                positions = arrayOfNulls(arity)
            } else {
                for (position in positions) position?.removeAll(stored, removed)
            }
        }

        fun position(index: Int): Position =
            positions[index] ?: Position(index).also { position ->
                for (i in 0 until all.size) position.add(all.items[i]!!)
                positions[index] = position
            }
    }

    private class Data(
        private val predicates: WordTable<Predicate> = WordTable(),
        private var nextId: Long = 0,
    ) {
        fun copy(): Data = Data(predicates.copy(), nextId)

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
            arity: Int,
        ): Predicate {
            val predicate = predicates[key]
            if (predicate?.owner === this) return predicate
            return (predicate?.copy(this) ?: Predicate(arity, this)).also { predicates[key] = it }
        }

        fun add(record: Record) {
            val head = record.head
            writable(headKey(record.words, head), arityOf(record, head)).add(Stored(nextId++, record))
        }

        fun removeAll(stored: List<Stored>) {
            for ((key, group) in stored.groupBy { headKey(it.record.words, it.record.head) }) {
                val record = group.first().record
                writable(key, arityOf(record, record.head)).removeAll(group)
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

        fun candidates(
            program: Program,
            words: IntArray,
            goal: Int,
        ): Candidates {
            val predicate = predicates[headKey(words, goal)] ?: return Candidates.NONE
            val all = predicate.all
            if (predicate.arity == 0 || all.size <= SCANNED) return all.candidates()
            var bestSize = all.size
            var best: Bucket? = all
            var other: Bucket? = null
            for (i in 0 until predicate.arity) {
                val argument = derefIn(words, goalArgument(words, goal, i))
                val key = topKey(words, argument)
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
                        bucket = position.groundByHash!![hash]
                        rest = position.nonGround
                        size = (bucket?.size ?: 0) + rest!!.size
                        if (size == 0) return Candidates.NONE
                    }
                }
                if (size < bestSize) {
                    bestSize = size
                    best = bucket
                    other = rest
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
        // At most this many clauses are tried in turn, with no index
        private const val SCANNED = 4

        // The candidates by top symbol from which a ground argument is hashed as a whole
        private const val GROUNDED_FROM = 8
        private const val MAX_DEPTH = 64
        private const val NOT_GROUND = Long.MIN_VALUE

        private fun Term.isRule() = this is Struct && functor == ":-" && arity == 2

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

        // The key of the predicate of a head: the functor word of a structure, the word of an atom, a key of its own for a list cell
        private fun headKey(
            words: IntArray,
            head: Int,
        ): Int =
            when (tag(head)) {
                STR -> words[payload(head)]
                LIST -> Program.LIST_KEY
                else -> head
            }

        private fun arityOf(
            record: Record,
            head: Int,
        ): Int =
            when (tag(head)) {
                STR -> record.program!!.arity(record.words[payload(head)])
                LIST -> 2
                else -> 0
            }

        private fun goalArgument(
            words: IntArray,
            goal: Int,
            i: Int,
        ): Int = if (tag(goal) == LIST) words[payload(goal) + i] else words[payload(goal) + 1 + i]

        private fun argumentOf(
            record: Record,
            i: Int,
        ): Int = derefIn(record.words, goalArgument(record.words, record.head, i))

        // The top symbol of a dereferenced argument of a head or of a goal, the same whether stored or on the heap: 0
        // for a variable, which has none
        private fun topKey(
            words: IntArray,
            argument: Int,
        ): Int =
            when (tag(argument)) {
                REF -> 0
                STR -> words[payload(argument)]
                LIST -> Program.LIST_KEY
                else -> argument
            }

        private fun mix(
            hash: Long,
            word: Int,
        ): Long = (hash xor word.toLong()) * -7046029254386353131L

        // A hash of the term [w0] of [words], the same wherever it is laid out; NOT_GROUND with a variable in it
        private fun groundHash(
            program: Program,
            words: IntArray,
            w0: Int,
        ): Long = hashInto(program, words, w0, 1125899906842597L)

        private fun hashInto(
            program: Program,
            words: IntArray,
            w0: Int,
            seed: Long,
        ): Long {
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
                        hash = hashInto(program, words, words[a], mix(hash, Program.LIST_KEY))
                        if (hash == NOT_GROUND) return NOT_GROUND
                        w = derefIn(words, words[a + 1])
                    }
                    else -> return mix(hash, w)
                }
            }
        }
    }
}
