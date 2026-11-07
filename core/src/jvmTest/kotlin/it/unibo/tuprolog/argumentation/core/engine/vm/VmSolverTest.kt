package it.unibo.tuprolog.argumentation.core.engine.vm

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.engine.Arg2pPrologSolverFactory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.Solver
import kotlin.test.Test
import kotlin.test.assertEquals

/** The solver of the machine on plain Prolog and on an argumentation theory */
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
        expected: List<String>,
    ) {
        assertEquals(expected, answers(Arg2pSolverFactory.default(theory), goal), "on $goal")
    }

    @Test
    fun plainProlog() {
        same("p(1). p(2). q(X) :- p(X), X > 1.", "q(X)", listOf("X = 2"))
        same(
            "app([], L, L). app([H|T], L, [H|R]) :- app(T, L, R).",
            "findall(X-Y, app(X, Y, [1, 2]), L)",
            listOf("L = ['-'([], [1, 2]), '-'([1], [2]), '-'([1, 2], [])]"),
        )
    }

    // A goal raising after it changed the dynamic theory leaves the solver's theory as it changed it, as a goal failing does
    @Test
    fun aHaltKeepsTheTheoryInStep() {
        val solver = Arg2pPrologSolverFactory.mutableSolverWithDefaultBuiltins()
        val halted = solver.solve(Struct.parse("assertz(f(1)), throw(oops)", solver.operators)).toList()
        assertEquals(1, halted.size)
        assertEquals(true, halted.single().isHalt)
        assertEquals(listOf("f(1)"), solver.dynamicKb.clauses.map { it.head.toString() }, "the solver's view of the theory")
        assertEquals(listOf("X = 1"), answers(solver, "f(X)"))
        assertEquals(listOf(""), answers(solver, "retract(f(1))"))
        assertEquals(emptyList(), answers(solver, "f(X)"))
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
        same(theory, "buildLabelSets(In, Out, Und)", listOf("In = [[b], [a]], Out = [['-'(a)]], Und = []"))
        same(theory, "answerQuery(b, In, Out, Und)", listOf("In = [b], Out = [], Und = []"))
    }
}
