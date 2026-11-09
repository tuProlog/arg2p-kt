package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.Arg2pPrologSolverFactory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.channel.OutputChannel
import it.unibo.tuprolog.solve.exception.Warning
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

// The goals of the directives see the theory, as the solver of tuProlog gives them: its answers on these theories are
// the ones expected here
class DirectivesTest {
    private val warnings = mutableListOf<Warning>()

    private fun seen(theory: String): List<String> =
        Arg2pPrologSolverFactory
            .mutableSolverWithDefaultBuiltins(
                staticKb = Theory.parse(theory),
                warnings = OutputChannel.of { warnings.add(it) },
            ).solve(Struct.parse("seen(X)"))
            .filter { it.isYes }
            .map { it.substitution.values.joinToString() }
            .toList()

    @Test
    fun anInitializationGoalReadsTheTheory() {
        assertEquals(listOf("1"), seen("p(1).\n:- initialization((p(X), assertz(seen(X)))).\n"))
        assertEquals(emptyList(), warnings)
    }

    @Test
    fun anInitializationGoalCallsARuleOfTheTheory() {
        assertEquals(listOf("1"), seen("q :- assertz(seen(1)).\n:- initialization(q).\n"))
        assertEquals(emptyList(), warnings)
    }

    // The goals run once the theory is loaded, wherever they are written
    @Test
    fun anInitializationGoalReadsTheFactsAfterIt() {
        assertEquals(listOf("1"), seen(":- initialization((p(X), assertz(seen(X)))).\np(1).\n"))
        assertEquals(emptyList(), warnings)
    }

    @Test
    fun aSolveDirectiveReadsTheTheory() {
        assertEquals(listOf("1"), seen("p(1).\n:- solve((p(X), assertz(seen(X)))).\n"))
        assertEquals(emptyList(), warnings)
    }

    @Test
    fun everySolutionOfAnInitializationGoalIsReached() {
        assertEquals(listOf("1", "2"), seen("p(1). p(2).\n:- initialization((p(X), assertz(seen(X)))).\n"))
        assertEquals(emptyList(), warnings)
    }

    @Test
    fun aFailingInitializationGoalWarns() {
        assertEquals(listOf("0"), seen(":- initialization(nothing).\nseen(0).\n"))
        assertEquals(1, warnings.size)
    }

    // Only initialization/1 and solve/1 run a goal, on the solver of tuProlog too
    @Test
    fun aPlainDirectiveDoesNotRun() {
        assertEquals(emptyList(), seen("p(1).\n:- p(X), assertz(seen(X)).\n"))
        assertEquals(emptyList(), warnings)
    }
}
