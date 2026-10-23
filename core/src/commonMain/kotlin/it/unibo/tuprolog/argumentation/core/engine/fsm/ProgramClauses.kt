package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.engine.ClauseSource
import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.solve.library.Runtime
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
        // the clauses of a single source, as it gives them; those of several, merged
        var clauses: List<Clause> = emptyList()
        var merged: ArrayList<Clause>? = null
        for (source in sources) {
            val found = source.clausesFor(goal, unificator)
            when {
                found.isEmpty() -> continue
                clauses.isEmpty() -> clauses = found
                merged == null -> merged = ArrayList(clauses).also { it.addAll(found) }.also { clauses = it }
                else -> merged.addAll(found)
            }
        }
        return clauses
    }

    companion object {
        /**
         * The clauses of the program of [context], with the [libraries] given (its own by default), as the indexes of
         * its solver keep them (see [TheoryIndexes.program])
         */
        fun of(
            context: PrologExecutionContext,
            libraries: Runtime = context.libraries,
        ): ProgramClauses =
            with(context) {
                theoryIndexes.program(libraries, staticKb, dynamicKb) {
                    of(libraries.asTheory(unificator), staticKb, dynamicKb, theoryIndexes)
                }
            }

        private fun of(
            libraries: Theory,
            staticKb: Theory,
            dynamicKb: Theory,
            indexes: TheoryIndexes,
        ): ProgramClauses = ProgramClauses(listOf(libraries, staticKb, dynamicKb).map { theory(it, indexes) })

        private fun theory(
            theory: Theory,
            indexes: TheoryIndexes,
        ): ClauseSource = if (theory.isMutable) MutableTheoryClauses(theory) else indexes.of(theory)
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
