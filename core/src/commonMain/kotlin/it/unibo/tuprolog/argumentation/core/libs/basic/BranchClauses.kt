package it.unibo.tuprolog.argumentation.core.libs.basic

import it.unibo.tuprolog.argumentation.core.engine.ClauseSource
import it.unibo.tuprolog.argumentation.core.engine.Delegation
import it.unibo.tuprolog.argumentation.core.engine.Inlining
import it.unibo.tuprolog.argumentation.core.engine.Sourcing
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Fact
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Rule
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Tuple
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.unify.Unificator

/**
 * The clauses of a branch of the context, as the arg2p engine resolves the lookups of context_check/1,2 with them
 * (see [ClauseStore.solve] for the lookups they stand for): the facts as stored, the rules with their bodies looked
 * up in the same branch (`h :- context_check(Branch, b)`), and for a `clause(Head, Body)` goal, a fact
 * `clause(h, b)` for each stored clause.
 */
internal class BranchClauses(
    private val store: ClauseStore,
    private val branch: Int,
) : ClauseSource {
    override fun clausesFor(
        goal: Struct,
        unificator: Unificator,
    ): List<Clause> =
        if (goal.isClauseLookup) {
            store.clausesFor(goal[0]).map { Fact.of(Struct.of(CLAUSE, it.head!!, it.body)) }
        } else {
            store.clausesFor(goal).map { if (it.body.isTrue) it else Rule.of(it.head!!, lookup(branch, it.body)) }
        }

    companion object {
        private const val CLAUSE = "clause"
        private const val LOOKUP = "context_check"

        private val Struct.isClauseLookup: Boolean
            get() = functor == CLAUSE && arity == 2

        /**
         * How the engine solves a lookup of [goal] in [branch], whose clauses are in [store]: resolved with its
         * [BranchClauses], or a conjunction as the lookup of each goal (against [libraries], the ones of the
         * lookup); null for the goals solved as the primitive does
         */
        fun delegation(
            store: ClauseStore,
            branch: Int,
            goal: Term,
            libraries: Runtime,
        ): Delegation? =
            when {
                goal !is Struct || goal.isTruth -> null
                goal is Tuple -> Inlining(lookup(branch, goal).castToStruct(), libraries)
                else -> Sourcing(goal, BranchClauses(store, branch))
            }

        // The lookup of each goal of a conjunction in a branch
        private fun lookup(
            branch: Int,
            goal: Term,
        ): Term =
            when {
                goal is Tuple -> Tuple.of(lookup(branch, goal.left), lookup(branch, goal.right))
                goal.isTrue -> goal
                else -> Struct.of(LOOKUP, Integer.of(branch), goal)
            }
    }
}
