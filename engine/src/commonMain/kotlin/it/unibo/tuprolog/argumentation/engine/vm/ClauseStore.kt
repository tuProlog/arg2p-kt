package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi
import it.unibo.tuprolog.argumentation.engine.term.Arg2pUnificator
import it.unibo.tuprolog.argumentation.engine.term.collectVariables
import it.unibo.tuprolog.argumentation.engine.term.renamedCopy
import it.unibo.tuprolog.argumentation.engine.words.IdSet
import it.unibo.tuprolog.argumentation.engine.words.Words.ATOM
import it.unibo.tuprolog.argumentation.engine.words.Words.LIST
import it.unibo.tuprolog.argumentation.engine.words.Words.REF
import it.unibo.tuprolog.argumentation.engine.words.Words.STR
import it.unibo.tuprolog.argumentation.engine.words.payload
import it.unibo.tuprolog.argumentation.engine.words.tag
import it.unibo.tuprolog.argumentation.engine.words.word
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
    private var data: StoreData,
    private var shared: Boolean,
    encoding: Program?,
    // The places the predicates (`functor/arity`) are looked up by, besides their arguments
    private val declared: Map<String, List<Place>>,
) {
    constructor(
        encoding: Program? = null,
        declared: Map<String, List<Place>> = emptyMap(),
    ) : this(StoreData(declared), false, encoding, declared)

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

    /** No clause any more */
    fun clear() {
        data = StoreData(declared)
        shared = false
    }

    /** The records re-encoded in the words of [program] (the machine's): kept as they are if they are its words already */
    fun bind(program: Program) {
        if (encoding === program) return
        val compatible = program.readsWordsOf(encoding)
        encoding = program
        if (data.isEmpty() || compatible) return
        val stored = data.all()
        data = StoreData(declared)
        shared = false
        for (record in stored) data.add(Record.encoded(record, program))
    }

    fun fork(): ClauseStore {
        shared = true
        return ClauseStore(data, true, encoding, declared)
    }

    private fun writable(): StoreData {
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
        if (tag(head) == REF || !isCallable(encoded, head)) encoded.clause
        writable().add(encoded)
    }

    // (the compound of a number no word holds is a number, see Numbers)
    private fun isCallable(
        record: Record,
        head: Int,
    ) = (tag(head) == STR && record.program!!.numberTag(record.words[payload(head)]) == 0) || tag(head) == LIST || tag(head) == ATOM

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
        assertA(Record(program, intArrayOf(program.functorWord(name, 1), program.smallWord(id)), word(STR, 0), null))
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

    companion object {
        // How deep the term API resolves a goal through the clauses
        private const val MAX_DEPTH = 64
    }
}
