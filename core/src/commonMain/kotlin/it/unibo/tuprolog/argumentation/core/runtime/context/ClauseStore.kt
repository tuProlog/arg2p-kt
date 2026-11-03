package it.unibo.tuprolog.argumentation.core.runtime.context

import it.unibo.tuprolog.argumentation.core.engine.Record
import it.unibo.tuprolog.argumentation.core.engine.vm.Program
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.LIST
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.REF
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.STR
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.payload
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.tag
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
 * Every argument position of a stored head is indexed by the preorder path of its words, up to the first variable:
 * lookups only consider the records whose paths are compatible with the goal's, and a ground argument is also found
 * by a hash of the whole of it.
 * Branches share their data until one of them is modified (copy-on-write), and then only copy the predicates they
 * modify. The term API (the methods on terms) serves the solvers that are not the machine, and the derived data.
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
        writable().add(Record.encoded(record, encoding()))
    }

    fun assertA(term: Term) = assertA(Record.of(term))

    /** [stored] removed, if it is here */
    fun remove(stored: Stored) {
        if (data.contains(stored)) writable().remove(stored)
    }

    /** The stored records whose head may unify with [head] (a record of the head alone), newest first */
    fun candidates(head: Record): List<Stored> = data.candidates(Record.encoded(head, encoding()))

    /**
     * A value derived from the clauses of the predicate [key] (`functor/arity`, newest first), named [name]: computed
     * once, until the predicate changes (the branches sharing the predicate share it)
     */
    fun <T : Any> derived(
        key: String,
        name: String,
        compute: (List<Clause>) -> T,
    ): T {
        val slash = key.lastIndexOf('/')
        val arity = key.substring(slash + 1).toInt()
        val name0 = key.substring(0, slash)
        val encoding = encoding()
        val functor = if (arity == 0) encoding.atomWord(name0) else encoding.functorWord(name0, arity)
        return data.derived(functor, name, compute)
    }

    // ---- the term API

    /** retractall: removes every clause whose head (and body, for a `:-` pattern) unifies with the given term */
    fun retractAll(term: Term) {
        val pattern = term.asStruct() ?: return
        val (head, body) = if (pattern.isRule()) pattern[0] to pattern[1] else pattern to null
        val headStruct = head.asStruct() ?: return
        val matching =
            candidates(Record.of(headStruct)).filter { stored ->
                val fresh = stored.clause.renamedCopy()
                val unifier = Arg2pUnificator.mgu(headStruct, fresh.head!!)
                unifier.isSuccess && (body == null || Arg2pUnificator.mgu(body.apply(unifier), fresh.body.apply(unifier)).isSuccess)
            }
        matching.forEach { remove(it) }
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

    // ---- the index, on the words of the records

    private class Node {
        val children = HashMap<Int, Node>()
        val here = LinkedHashSet<Long>()

        fun copy(): Node =
            Node().also { node ->
                node.here.addAll(here)
                children.forEach { (key, child) -> node.children[key] = child.copy() }
            }

        fun collectAll(target: MutableSet<Long>) {
            target.addAll(here)
            children.values.forEach { it.collectAll(target) }
        }

        fun add(
            path: Path,
            id: Long,
        ) {
            var node = this
            for (i in 0 until path.size) node = node.children.getOrPut(path.symbols[i]) { Node() }
            node.here.add(id)
        }

        fun remove(
            path: Path,
            id: Long,
        ) {
            var node = this
            for (i in 0 until path.size) node = node.children[path.symbols[i]] ?: return
            node.here.remove(id)
        }

        /** Ids of the stored arguments whose path is compatible with the given one */
        fun compatible(
            path: Path,
            target: MutableSet<Long>,
        ) {
            var node: Node? = this
            for (i in 0 until path.size) {
                // stored arguments with a variable (or truncated) at this point are compatible
                target.addAll(node!!.here)
                node = node.children[path.symbols[i]]
                if (node == null) break
            }
            // stored arguments extending the (truncated or variable-ending) query path are compatible
            if (node != null) {
                if (path.complete) target.addAll(node.here) else node.collectAll(target)
            }
        }
    }

    /**
     * Index of an argument position: ground arguments are looked up exactly, by a hash of the whole of them, the
     * others by the path of their words (which is truncated, so that it would not tell apart large terms sharing a
     * prefix). The paths of the ground arguments are only needed by partially instantiated lookups: they are
     * indexed on the first of them, so that positions only looked up by ground terms or variables (as labels) do not
     * pay for them.
     */
    private class Position {
        val ground = HashMap<Long, LinkedHashSet<Long>>()
        val nonGround = Node()
        private var groundPaths: Node? = null

        fun copy(): Position =
            Position().also { position ->
                ground.forEach { (hash, ids) -> position.ground[hash] = LinkedHashSet(ids) }
                position.nonGround.children.putAll(nonGround.copy().children)
                position.nonGround.here.addAll(nonGround.here)
                position.groundPaths = groundPaths?.copy()
            }

        fun add(
            arg: Argument,
            id: Long,
        ) {
            if (arg.ground) {
                ground.getOrPut(arg.hash) { LinkedHashSet() }.add(id)
                groundPaths?.add(arg.path, id)
            } else {
                nonGround.add(arg.path, id)
            }
        }

        fun remove(
            arg: Argument,
            id: Long,
        ) {
            if (arg.ground) {
                ground[arg.hash]?.let {
                    it.remove(id)
                    if (it.isEmpty()) ground.remove(arg.hash)
                }
                groundPaths?.remove(arg.path, id)
            } else {
                nonGround.remove(arg.path, id)
            }
        }

        // built from the ground arguments stored so far, then kept up to date: a cache, also on shared data
        private fun groundPaths(
            predicate: Predicate,
            position: Int,
        ): Node =
            groundPaths ?: Node().also { node ->
                for (stored in predicate.clauses.values) {
                    val arg = predicate.arguments(stored)[position]
                    if (arg.ground) node.add(arg.path, stored.id)
                }
                groundPaths = node
            }

        /** Ids of the stored arguments which may unify with the given one, or null when it does not restrict them */
        fun candidates(
            arg: Argument,
            predicate: Predicate,
            position: Int,
        ): Set<Long>? {
            if (arg.path.size == 0) return null
            val found = LinkedHashSet<Long>()
            if (arg.ground) {
                ground[arg.hash]?.let { found.addAll(it) }
            } else {
                groundPaths(predicate, position).compatible(arg.path, found)
            }
            nonGround.compatible(arg.path, found)
            return found
        }
    }

    /** The preorder words of an argument up to its first variable, complete when the whole of it was visited */
    private class Path(
        val symbols: IntArray,
        val size: Int,
        val complete: Boolean,
    )

    /** An argument of a head, as the index sees it: its path, and the hash of the whole of it if it is ground */
    private class Argument(
        val path: Path,
        val ground: Boolean,
        val hash: Long,
    )

    private class Predicate(
        arity: Int,
    ) {
        val clauses = LinkedHashMap<Long, Stored>()
        val indexes = Array(arity) { Position() }

        // Whole ground heads, for the exact lookup of ground goals
        val groundHeads = HashMap<Long, LinkedHashSet<Long>>()
        val nonGroundHeads = LinkedHashSet<Long>()

        // The values derived from the clauses (see ClauseStore.derived), until they change
        val derived = HashMap<String, Any>()

        // The arguments of the stored heads as the index sees them, computed once
        private val arguments = HashMap<Long, Array<Argument>>()

        fun arguments(stored: Stored): Array<Argument> = arguments.getOrPut(stored.id) { argumentsOf(stored.record) }

        fun copy(): Predicate =
            Predicate(indexes.size).also { predicate ->
                predicate.clauses.putAll(clauses)
                indexes.forEachIndexed { i, position -> predicate.indexes[i] = position.copy() }
                groundHeads.forEach { (hash, ids) -> predicate.groundHeads[hash] = LinkedHashSet(ids) }
                predicate.nonGroundHeads.addAll(nonGroundHeads)
                predicate.arguments.putAll(arguments)
            }
    }

    private class Data(
        private val predicates: HashMap<Int, Predicate> = HashMap(),
        private var nextId: Long = 0,
    ) {
        // Predicates this data can modify in place, the others are shared with the data it was copied from
        // (shared data is never modified: a branch copies it before its first change)
        private val owned = HashSet<Int>()

        fun copy(): Data = Data(HashMap(predicates), nextId)

        fun isEmpty(): Boolean = predicates.values.all { it.clauses.isEmpty() }

        /** Every record, oldest first */
        fun all(): List<Record> =
            predicates.values
                .flatMap { it.clauses.values }
                .sortedBy { it.id }
                .map { it.record }

        fun contains(stored: Stored): Boolean = predicates[headKey(stored.record)]?.clauses?.containsKey(stored.id) == true

        /** The predicate stored under key, copied the first time this data modifies it */
        private fun writable(
            key: Int,
            arity: Int,
        ): Predicate {
            val predicate = predicates[key]
            if (!owned.add(key) && predicate != null) return predicate
            return (predicate?.copy() ?: Predicate(arity)).also { predicates[key] = it }
        }

        fun add(record: Record) {
            val stored = Stored(nextId++, record)
            val predicate = writable(headKey(record), headArity(record))
            predicate.derived.clear()
            predicate.clauses[stored.id] = stored
            val arguments = predicate.arguments(stored)
            arguments.forEachIndexed { i, arg -> predicate.indexes[i].add(arg, stored.id) }
            if (arguments.all { it.ground }) {
                predicate.groundHeads.getOrPut(headHash(arguments)) { LinkedHashSet() }.add(stored.id)
            } else {
                predicate.nonGroundHeads.add(stored.id)
            }
        }

        fun remove(stored: Stored) {
            val record = stored.record
            val predicate = writable(headKey(record), headArity(record))
            predicate.derived.clear()
            predicate.clauses.remove(stored.id)
            val arguments = predicate.arguments(stored)
            arguments.forEachIndexed { i, arg -> predicate.indexes[i].remove(arg, stored.id) }
            if (arguments.all { it.ground }) {
                val hash = headHash(arguments)
                predicate.groundHeads[hash]?.let {
                    it.remove(stored.id)
                    if (it.isEmpty()) predicate.groundHeads.remove(hash)
                }
            } else {
                predicate.nonGroundHeads.remove(stored.id)
            }
        }

        fun <T : Any> derived(
            key: Int,
            name: String,
            compute: (List<Clause>) -> T,
        ): T {
            val predicate = predicates[key] ?: return compute(emptyList())
            @Suppress("UNCHECKED_CAST")
            return predicate.derived.getOrPut(name) {
                val clauses = predicate.clauses.values.map { it.clause }
                compute(clauses.asReversed())
            } as T
        }

        /** Stored records whose head may unify with [head], newest first */
        fun candidates(head: Record): List<Stored> {
            val predicate = predicates[headKey(head)] ?: return emptyList()
            val arguments = argumentsOf(head)
            if (arguments.all { it.ground } && predicate.nonGroundHeads.size <= MAX_NON_GROUND_SCAN) {
                val ids = LinkedHashSet<Long>()
                predicate.groundHeads[headHash(arguments)]?.let { ids.addAll(it) }
                ids.addAll(predicate.nonGroundHeads)
                return ids.sortedDescending().mapNotNull { predicate.clauses[it] }
            }
            var best: Set<Long>? = null
            arguments.forEachIndexed { i, arg ->
                val found = predicate.indexes[i].candidates(arg, predicate, i) ?: return@forEachIndexed
                if (best.let { it == null || found.size < it.size }) best = found
            }
            // all the clauses: in the order of their ids (they are added in that order), the newest first
            val ids =
                best ?: return predicate.clauses.values
                    .toList()
                    .asReversed()
            return ids.sortedDescending().mapNotNull { predicate.clauses[it] }
        }
    }

    companion object {
        private const val MAX_PATH = 16

        // Ground goals are looked up by their whole head, plus every stored clause with a non-ground head: when
        // these are more, the lookup goes through the argument indexes instead
        private const val MAX_NON_GROUND_SCAN = 8
        private const val MAX_DEPTH = 64

        private fun Term.isRule() = this is Struct && functor == ":-" && arity == 2

        // The key of the predicate of a record's head: the functor word of a structure, the word of an atom (or of a
        // number, head or not), a key of its own for a list cell
        private fun headKey(record: Record): Int {
            val head = record.head
            return when (tag(head)) {
                STR -> record.words[payload(head)]
                LIST -> Program.LIST_KEY
                else -> head
            }
        }

        private fun headArity(record: Record): Int {
            val head = record.head
            return when (tag(head)) {
                STR -> record.program!!.arity(record.words[payload(head)])
                LIST -> 2
                else -> 0
            }
        }

        private fun headHash(arguments: Array<Argument>): Long {
            var h = 17L
            for (arg in arguments) h = h * 31 + arg.hash
            return h
        }

        // The arguments of the head of [record] (encoded), as the index sees them
        private fun argumentsOf(record: Record): Array<Argument> {
            val head = record.head
            val words = record.words
            return when (tag(head)) {
                STR -> {
                    val a = payload(head)
                    val n = record.program!!.arity(words[a])
                    Array(n) { argumentOf(record, words[a + 1 + it]) }
                }
                LIST -> {
                    val a = payload(head)
                    arrayOf(argumentOf(record, words[a]), argumentOf(record, words[a + 1]))
                }
                else -> emptyArray()
            }
        }

        private fun argumentOf(
            record: Record,
            w: Int,
        ): Argument {
            val words = record.words
            val program = record.program!!
            // The path: preorder words up to the first variable, at most MAX_PATH; the hash of the whole, and whether
            // it is ground, by a full visit
            val symbols = IntArray(MAX_PATH)
            var size = 0
            var complete = true
            var ground = true
            var hash = 7L
            val stack = IntArray(64).let { ArrayDequeInt(it) }
            stack.push(w)
            var visited = 0
            var pathOpen = true
            while (stack.isNotEmpty()) {
                val current = stack.pop()
                when (tag(current)) {
                    REF -> {
                        ground = false
                        if (pathOpen) {
                            complete = false
                            pathOpen = false
                        }
                        hash = hash * 31 + 1
                        continue
                    }
                    STR -> {
                        val a = payload(current)
                        val functor = words[a]
                        if (pathOpen) {
                            if (visited == MAX_PATH) {
                                complete = false
                                pathOpen = false
                            } else {
                                symbols[size++] = functor
                                visited++
                            }
                        }
                        hash = hash * 31 + functor
                        val n = program.arity(functor)
                        for (i in n downTo 1) stack.push(words[a + i])
                    }
                    LIST -> {
                        val a = payload(current)
                        if (pathOpen) {
                            if (visited == MAX_PATH) {
                                complete = false
                                pathOpen = false
                            } else {
                                symbols[size++] = Program.LIST_KEY
                                visited++
                            }
                        }
                        hash = hash * 31 + Program.LIST_KEY
                        stack.push(words[a + 1])
                        stack.push(words[a])
                    }
                    else -> {
                        if (pathOpen) {
                            if (visited == MAX_PATH) {
                                complete = false
                                pathOpen = false
                            } else {
                                symbols[size++] = current
                                visited++
                            }
                        }
                        hash = hash * 31 + current
                    }
                }
            }
            return Argument(Path(symbols, size, complete), ground, hash)
        }
    }

    // A stack of ints
    private class ArrayDequeInt(
        private var items: IntArray,
    ) {
        private var size = 0

        fun isNotEmpty() = size > 0

        fun push(v: Int) {
            if (size == items.size) items = items.copyOf(size * 2)
            items[size++] = v
        }

        fun pop(): Int = items[--size]
    }
}
