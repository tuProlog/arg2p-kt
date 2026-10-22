package it.unibo.tuprolog.argumentation.core.libs.basic

import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Fact
import it.unibo.tuprolog.core.Numeric
import it.unibo.tuprolog.core.Rule
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Truth
import it.unibo.tuprolog.core.Var

/**
 * Clause database backing the argumentation context.
 *
 * It replaces a full Prolog solver per context branch: clauses are only ever asserted (asserta order),
 * retracted, and looked up as facts or through clause/2, so a dedicated store suffices.
 * Every argument position of a stored head is indexed by the preorder path of its symbols, up to the
 * first variable: lookups only unify the clauses whose paths are compatible with the query ones.
 * Branches share their data until one of them is modified (copy-on-write), and then only copy the predicates they modify.
 */
class ClauseStore private constructor(
    private var data: Data,
    private var shared: Boolean,
) {
    constructor() : this(Data(), false)

    fun fork(): ClauseStore {
        shared = true
        return ClauseStore(data, true)
    }

    private fun writable(): Data {
        if (shared) {
            data = data.copy()
            shared = false
        }
        return data
    }

    /** asserta: the new clause is returned before the existing ones */
    fun assertA(term: Term) {
        writable().add(toClause(term))
    }

    /** retractall: removes every clause whose head (and body, for a `:-` pattern) unifies with the given term */
    fun retractAll(term: Term) {
        val pattern = term.asStruct() ?: return
        val (head, body) = if (pattern.isRule()) pattern[0] to pattern[1] else pattern to null
        val headStruct = head.asStruct() ?: return
        val matching =
            data.candidates(headStruct).filter { stored ->
                val fresh = stored.clause.freshCopy()
                val unifier = Arg2pUnificator.mgu(headStruct, fresh.head!!)
                unifier.isSuccess && (body == null || Arg2pUnificator.mgu(body.apply(unifier), fresh.body.apply(unifier)).isSuccess)
            }
        if (matching.isNotEmpty()) {
            val target = writable()
            matching.forEach { target.remove(it) }
        }
    }

    /** The stored clauses whose head may unify with the given one, newest first, as stored */
    fun clausesFor(head: Term): List<Clause> = head.asStruct()?.let { struct -> data.candidates(struct).map { it.clause } } ?: emptyList()

    /** Solutions of a goal (a fact, a single-goal rule, or `clause(Head, Body)`) on a snapshot of the store */
    fun solve(goal: Term): Sequence<Substitution> {
        val struct = goal.asStruct() ?: return emptySequence()
        val variables = struct.variables.toSet()
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
        val variables = body.variables.toSet()
        return data.candidates(headStruct).asSequence().mapNotNull { stored ->
            val fresh = stored.clause.freshCopy()
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
        return data.candidates(headStruct).asSequence().mapNotNull { stored ->
            val fresh = stored.clause.freshCopy()
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
        return data.candidates(struct).asSequence().flatMap { stored ->
            val fresh = stored.clause.freshCopy()
            val step = Arg2pUnificator.mgu(struct, fresh.head!!)
            if (!step.isSuccess) {
                emptySequence()
            } else {
                val composed = (unifier + step).takeIf { it.isSuccess } ?: return@flatMap emptySequence<Substitution>()
                resolve(fresh.body, composed, depth + 1)
            }
        }
    }

    private class Stored(
        val id: Long,
        val clause: Clause,
    )

    private class Node {
        val children = HashMap<String, Node>()
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
            arg: Term,
            id: Long,
        ) {
            var node = this
            path(arg).first.forEach { symbol -> node = node.children.getOrPut(symbol) { Node() } }
            node.here.add(id)
        }

        fun remove(
            arg: Term,
            id: Long,
        ) {
            var node = this
            for (symbol in path(arg).first) node = node.children[symbol] ?: return
            node.here.remove(id)
        }

        /** Ids of the stored arguments whose path is compatible with the given one */
        fun compatible(
            symbols: List<String>,
            complete: Boolean,
            target: MutableSet<Long>,
        ) {
            var node: Node? = this
            for (symbol in symbols) {
                // stored arguments with a variable (or truncated) at this point are compatible
                target.addAll(node!!.here)
                node = node.children[symbol]
                if (node == null) break
            }
            // stored arguments extending the (truncated or variable-ending) query path are compatible
            if (node != null) {
                if (complete) target.addAll(node.here) else node.collectAll(target)
            }
        }
    }

    /**
     * Index of an argument position: ground arguments are looked up exactly, by the term itself, the others by the
     * path of their symbols (which is truncated, so that it would not tell apart large terms sharing a prefix).
     * The paths of the ground arguments are only needed by partially instantiated lookups: they are indexed on the
     * first of them, so that positions only looked up by ground terms or variables (as labels) do not pay for them.
     */
    private class Position {
        val ground = HashMap<Term, LinkedHashSet<Long>>()
        val nonGround = Node()
        private var groundPaths: Node? = null

        fun copy(): Position =
            Position().also { position ->
                ground.forEach { (term, ids) -> position.ground[term] = LinkedHashSet(ids) }
                position.nonGround.children.putAll(nonGround.copy().children)
                position.nonGround.here.addAll(nonGround.here)
                position.groundPaths = groundPaths?.copy()
            }

        fun add(
            arg: Term,
            id: Long,
        ) {
            if (arg.isGround) {
                ground.getOrPut(arg) { LinkedHashSet() }.add(id)
                groundPaths?.add(arg, id)
            } else {
                nonGround.add(arg, id)
            }
        }

        fun remove(
            arg: Term,
            id: Long,
        ) {
            if (arg.isGround) {
                ground[arg]?.let {
                    it.remove(id)
                    if (it.isEmpty()) ground.remove(arg)
                }
                groundPaths?.remove(arg, id)
            } else {
                nonGround.remove(arg, id)
            }
        }

        // built from the ground arguments stored so far, then kept up to date: a cache, also on shared data
        private fun groundPaths(): Node =
            groundPaths ?: Node().also { node ->
                ground.forEach { (term, ids) -> ids.forEach { node.add(term, it) } }
                groundPaths = node
            }

        /** Ids of the stored arguments which may unify with the given one, or null when it does not restrict them */
        fun candidates(arg: Term): Set<Long>? {
            val (symbols, complete) = path(arg)
            if (symbols.isEmpty()) return null
            val found = LinkedHashSet<Long>()
            if (arg.isGround) {
                ground[arg]?.let { found.addAll(it) }
            } else {
                groundPaths().compatible(symbols, complete, found)
            }
            nonGround.compatible(symbols, complete, found)
            return found
        }
    }

    private class Predicate(
        arity: Int,
    ) {
        val clauses = LinkedHashMap<Long, Stored>()
        val indexes = Array(arity) { Position() }

        // Whole ground heads, for the exact lookup of ground goals
        val groundHeads = HashMap<Term, LinkedHashSet<Long>>()
        val nonGroundHeads = LinkedHashSet<Long>()

        fun copy(): Predicate =
            Predicate(indexes.size).also { predicate ->
                predicate.clauses.putAll(clauses)
                indexes.forEachIndexed { i, position -> predicate.indexes[i] = position.copy() }
                groundHeads.forEach { (head, ids) -> predicate.groundHeads[head] = LinkedHashSet(ids) }
                predicate.nonGroundHeads.addAll(nonGroundHeads)
            }
    }

    private class Data(
        private val predicates: HashMap<String, Predicate> = HashMap(),
        private var nextId: Long = 0,
    ) {
        // Predicates this data can modify in place, the others are shared with the data it was copied from
        // (shared data is never modified: a branch copies it before its first change)
        private val owned = HashSet<String>()

        fun copy(): Data = Data(HashMap(predicates), nextId)

        /** The predicate stored under key, copied the first time this data modifies it */
        private fun writable(
            key: String,
            arity: Int,
        ): Predicate {
            val predicate = predicates[key]
            if (!owned.add(key) && predicate != null) return predicate
            return (predicate?.copy() ?: Predicate(arity)).also { predicates[key] = it }
        }

        fun add(clause: Clause) {
            val head = clause.head!!
            val stored = Stored(nextId++, clause)
            val predicate = writable(key(head), head.arity)
            predicate.clauses[stored.id] = stored
            head.args.forEachIndexed { i, arg -> predicate.indexes[i].add(arg, stored.id) }
            if (head.isGround) {
                predicate.groundHeads.getOrPut(head) { LinkedHashSet() }.add(stored.id)
            } else {
                predicate.nonGroundHeads.add(stored.id)
            }
        }

        fun remove(stored: Stored) {
            val head = stored.clause.head!!
            if (predicates[key(head)]?.clauses?.containsKey(stored.id) != true) return
            val predicate = writable(key(head), head.arity)
            predicate.clauses.remove(stored.id)
            head.args.forEachIndexed { i, arg -> predicate.indexes[i].remove(arg, stored.id) }
            if (head.isGround) {
                predicate.groundHeads[head]?.let {
                    it.remove(stored.id)
                    if (it.isEmpty()) predicate.groundHeads.remove(head)
                }
            } else {
                predicate.nonGroundHeads.remove(stored.id)
            }
        }

        /** Stored clauses whose head may unify with the given one, newest first */
        fun candidates(head: Struct): List<Stored> {
            val predicate = predicates[key(head)] ?: return emptyList()
            if (head.isGround && predicate.nonGroundHeads.size <= MAX_NON_GROUND_SCAN) {
                val ids = LinkedHashSet<Long>()
                predicate.groundHeads[head]?.let { ids.addAll(it) }
                ids.addAll(predicate.nonGroundHeads)
                return ids.sortedDescending().mapNotNull { predicate.clauses[it] }
            }
            var best: Set<Long>? = null
            head.args.forEachIndexed { i, arg ->
                val found = predicate.indexes[i].candidates(arg) ?: return@forEachIndexed
                if (best == null || found.size < best!!.size) best = found
            }
            val ids = best ?: predicate.clauses.keys
            return ids.sortedDescending().mapNotNull { predicate.clauses[it] }
        }

        private fun key(head: Struct) = "${head.functor}/${head.arity}"
    }

    companion object {
        private const val MAX_PATH = 16

        // Ground goals are looked up by their whole head, plus every stored clause with a non-ground head: when
        // these are more, the lookup goes through the argument indexes instead
        private const val MAX_NON_GROUND_SCAN = 8
        private const val MAX_DEPTH = 64

        private fun Term.isRule() = this is Struct && functor == ":-" && arity == 2

        private fun toClause(term: Term): Clause {
            val struct = term.castToStruct()
            return if (struct.isRule()) Rule.of(struct[0].castToStruct(), struct[1]) else Fact.of(struct)
        }

        private fun symbol(term: Term): String =
            when (term) {
                is Numeric -> "#${term.decimalValue}"
                is Atom -> "a:${term.value}"
                is Struct -> "s:${term.functor}/${term.arity}"
                else -> error("unexpected term $term")
            }

        /** Preorder symbols of a term up to its first variable; complete when the term was fully visited */
        private fun path(term: Term): Pair<List<String>, Boolean> {
            val symbols = ArrayList<String>()
            val stack = ArrayDeque<Term>().apply { addLast(term) }
            while (stack.isNotEmpty()) {
                if (symbols.size == MAX_PATH) return symbols to false
                val current = stack.removeLast()
                if (current is Var) return symbols to false
                symbols.add(symbol(current))
                if (current is Struct && current !is Atom) {
                    for (i in current.arity - 1 downTo 0) stack.addLast(current[i])
                }
            }
            return symbols to true
        }
    }
}
