package it.unibo.tuprolog.argumentation.core.engine.vm

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.Solver
import kotlin.test.Test
import kotlin.test.assertEquals

/** The solver of the machine against the engine, on argumentation theories: the same answers */
class VmSolverTest {
    private fun answers(
        solver: Solver,
        goal: String,
    ): List<String> =
        solver
            .solve(Struct.parse(goal, solver.operators))
            .filterIsInstance<Solution.Yes>()
            .map { s ->
                s.substitution.entries
                    .sortedBy { it.key.name }
                    .joinToString(", ") { (v, t) -> "${v.name} = $t" }
            }.toList()

    private fun same(
        theory: String,
        goal: String,
    ) {
        val expected = answers(Arg2pSolverFactory.default(theory), goal)
        val actual = answers(Arg2pSolverFactory.default(theory, vm = true), goal)
        assertEquals(expected, actual, "on $goal")
    }

    @Test
    fun plainProlog() {
        same("p(1). p(2). q(X) :- p(X), X > 1.", "q(X)")
        same("app([], L, L). app([H|T], L, [H|R]) :- app(T, L, R).", "findall(X-Y, app(X, Y, [1, 2]), L)")
    }

    @Test
    fun argumentation() {
        val theory =
            """
            r1 : [] => a.
            r2 : [] => -a.
            r3 : a => b.
            sup(r1, r2).
            """.trimIndent()
        same(theory, "buildLabelSets(In, Out, Und)")
        same(theory, "answerQuery(b, In, Out, Und)")
    }
}
