package it.unibo.tuprolog.argumentation.core.engine

import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.unify.Unificator

/**
 * Where the engine finds the clauses it resolves a goal with (arg2p): the theories of the program, or the source of
 * a [Sourcing] delegation.
 */
fun interface ClauseSource {
    /**
     * The clauses whose heads may unify with [goal] (through [unificator]), in their order, as they are when called:
     * the resolution does not see the clauses added or removed afterwards (logical update view)
     */
    fun clausesFor(
        goal: Struct,
        unificator: Unificator,
    ): List<Clause>
}
