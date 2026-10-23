package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.engine.Arg2pPrologSolverFactory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.TrackVariables
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

// The bindings of a resolution, trimmed when a sub-goal ends, keep those the goals left to solve use
class SubstitutionTrimmingTest {
    // Y is bound by a/1 and used by c/2 only: count/1, in between, makes the substitution large enough to be trimmed
    private val theory =
        """
        a(1).
        c(Y, Y).
        count(0) :- !.
        count(N) :- M is N - 1, count(M).
        p(X) :- a(Y), count(100), c(Y, X).
        """.trimIndent()

    private fun solve(flags: FlagStore): String {
        val solver = Arg2pPrologSolverFactory.solverWithDefaultBuiltins(flags = flags)
        val theory = Theory.parse(theory, solver.operators)
        val withTheory = Arg2pPrologSolverFactory.solverWithDefaultBuiltins(flags = flags, staticKb = theory)
        return withTheory
            .solve(Struct.parse("p(X)", solver.operators))
            .first()
            .substitution
            .getByName("X")
            .toString()
    }

    @Test
    fun aBindingUsedByAGoalLeftIsKept() {
        assertEquals("1", solve(FlagStore.DEFAULT))
        assertEquals("1", solve(FlagStore.DEFAULT.set(TrackVariables, TrackVariables.ON)))
    }
}
