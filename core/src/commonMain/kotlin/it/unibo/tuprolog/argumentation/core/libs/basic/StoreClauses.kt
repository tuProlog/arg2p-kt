package it.unibo.tuprolog.argumentation.core.libs.basic

import it.unibo.tuprolog.argumentation.core.engine.ClauseSource
import it.unibo.tuprolog.argumentation.core.engine.Delegation
import it.unibo.tuprolog.argumentation.core.engine.Inlining
import it.unibo.tuprolog.argumentation.core.engine.Sourcing
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Fact
import it.unibo.tuprolog.core.Rule
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Tuple
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.unify.Unificator

/** The goal looking up a goal in a store of clauses (as `context_check(Branch, Goal)` or `cache_check(Goal)`) */
internal fun interface StoreLookup {
    fun of(goal: Term): Struct
}

/**
 * The clauses of a [ClauseStore] (a branch of the context, the cache), as the arg2p engine resolves the lookups of
 * the store with them (see [ClauseStore.solve] for the answers they stand for): the facts as stored, the rules with
 * their bodies looked up in the same store (`h :- Lookup(b)`, see [StoreLookup]), and for a `clause(Head, Body)`
 * goal, a fact `clause(h, b)` for each stored clause.
 */
internal class StoreClauses(
    private val store: ClauseStore,
    private val lookup: StoreLookup,
) : ClauseSource {
    override fun clausesFor(
        goal: Struct,
        unificator: Unificator,
    ): List<Clause> =
        if (goal.isClauseLookup) {
            store.clausesFor(goal[0]).map { Fact.of(Struct.of(CLAUSE, it.head!!, it.body)) }
        } else {
            store.clausesFor(goal).map { if (it.body.isTrue) it else Rule.of(it.head!!, lookup.ofEach(it.body)) }
        }

    companion object {
        private const val CLAUSE = "clause"

        private val Struct.isClauseLookup: Boolean
            get() = functor == CLAUSE && arity == 2

        /**
         * How the engine solves the lookup of [goal] in [store] (through [lookup]): resolved with its [StoreClauses],
         * or a conjunction as the lookup of each goal (against [libraries], the ones of the lookup); null for the
         * goals solved as the primitive does
         */
        fun delegation(
            store: ClauseStore,
            lookup: StoreLookup,
            goal: Term,
            libraries: Runtime,
        ): Delegation? =
            when {
                goal !is Struct || goal.isTruth -> null
                goal is Tuple -> Inlining(lookup.ofEach(goal).castToStruct(), libraries)
                else -> Sourcing(goal, StoreClauses(store, lookup))
            }

        // The lookup of each goal of a conjunction
        private fun StoreLookup.ofEach(goal: Term): Term =
            when {
                goal is Tuple -> Tuple.of(ofEach(goal.left), ofEach(goal.right))
                goal.isTrue -> goal
                else -> of(goal)
            }
    }
}
