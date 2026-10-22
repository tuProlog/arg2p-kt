package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.engine.ClauseSource
import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.unify.Unificator

/**
 * The clauses of the program of a resolution, from the libraries, the static and the dynamic theory, in this order
 * (arg2p).
 */
internal class ProgramClauses private constructor(
    private val sources: List<ClauseSource>,
) : ClauseSource {
    override fun clausesFor(
        goal: Struct,
        unificator: Unificator,
    ): List<Clause> {
        val clauses = ArrayList<Clause>()
        sources.forEach { clauses.addAll(it.clausesFor(goal, unificator)) }
        return clauses
    }

    companion object {
        /** The clauses of the program of [context] */
        fun of(context: PrologExecutionContext): ProgramClauses =
            with(context) {
                ProgramClauses(listOf(theory(libraries.asTheory(unificator)), theory(staticKb), theory(dynamicKb)))
            }

        private fun theory(theory: Theory): ClauseSource =
            if (theory.isMutable) MutableTheoryClauses(theory) else IndexedTheoryClauses.of(theory)
    }
}

/** The clauses of a mutable theory, as it is when looked up */
internal class MutableTheoryClauses(
    private val theory: Theory,
) : ClauseSource {
    override fun clausesFor(
        goal: Struct,
        unificator: Unificator,
    ): List<Clause> = theory[goal].toList()
}
