package it.unibo.tuprolog.argumentation.core.runtime.context

import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term

/**
 * The branches of a context: each a store of clauses (see [ClauseStore]), one of them active, a new one forked from
 * another; and the identifiers of the terms (context_id/2), shared by all the branches. Branch 0 is the root.
 */
internal class ContextBranches {
    private var next: Int = 1
    private val stores: MutableMap<Int, ClauseStore> = mutableMapOf(0 to ClauseStore())
    private val identifiers: MutableMap<Term, Int> = mutableMapOf()

    /** The active branch */
    var active: Int = 0
        private set

    /** The store of the active branch */
    val selected: ClauseStore
        get() = stores[active]!!

    /** The branch the next one created will be */
    val nextBranch: Int
        get() = next

    /** The most recently created branch */
    val last: Int
        get() = next - 1

    /** The store of [branch]; null if there is no such branch (never created, or dropped) */
    fun storeOf(branch: Int): ClauseStore? = stores[branch]

    /** Back to a single empty branch, the root, and no identifiers */
    fun reset() {
        active = 0
        next = 1
        stores.clear()
        stores[0] = ClauseStore()
        identifiers.clear()
    }

    /** [branch] made active; false if there is no such branch */
    fun checkout(branch: Int): Boolean {
        if (branch !in stores) return false
        active = branch
        return true
    }

    /** A new branch forked from [from], made active; null if there is no branch [from] */
    fun branch(from: Int): Int? {
        val source = stores[from] ?: return null
        stores[next] = source.fork()
        active = next++
        return active
    }

    /** [branch] dropped; false for the active one */
    fun drop(branch: Int): Boolean {
        if (branch == active) return false
        stores.remove(branch)
        return true
    }

    /** The branches created after [after] dropped, but those in [keep] and the active one */
    fun dropAfter(
        after: Int,
        keep: Set<Int>,
    ) {
        stores.keys.removeAll { it > after && it !in keep && it != active }
    }

    /**
     * The identifier of [term], shared by all the copies of the term which only differ in variable names (variables
     * get renamed whenever a term is read back from the context)
     */
    fun identifier(term: Term): Int = identifiers.getOrPut(canonical(term)) { identifiers.size }

    private fun canonical(term: Term): Term {
        val variables = term.variables.distinct().toList()
        if (variables.isEmpty()) return term
        return term.apply(Substitution.of(variables.mapIndexed { i, v -> v to Atom.of("_V$i") }.toMap()))
    }
}
