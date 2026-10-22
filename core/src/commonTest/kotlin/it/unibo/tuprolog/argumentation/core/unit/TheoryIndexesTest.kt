package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.argumentation.core.engine.AbstractPrologSolver
import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.argumentation.core.engine.fsm.TheoryIndexes
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertNotSame
import kotlin.test.assertSame

/**
 * The indexes of the immutable theories ([TheoryIndexes]): each
 * solver keeps its own across its resolutions, and shares them with the solvers its resolutions create.
 */
class TheoryIndexesTest {
    private fun indexesOf(solver: Any) = (solver as AbstractPrologSolver).theoryIndexes

    private fun solve(
        solver: Any,
        goal: String,
    ) {
        val prolog = solver as AbstractPrologSolver
        prolog.solve(Struct.parse(goal, prolog.operators)).first()
    }

    @Test
    fun eachSolverHasItsOwn() {
        assertNotSame(indexesOf(solverWithTheory("p(1).")), indexesOf(solverWithTheory("p(1).")))
    }

    @Test
    fun aSolverKeepsThemAcrossItsResolutions() {
        val solver = solverWithTheory("p(1).")
        val indexes = indexesOf(solver)
        solve(solver, "p(X)")
        solve(solver, "findall(X, p(X), _)")
        assertSame(indexes, indexesOf(solver))
    }

    @Test
    fun theSolversOfAResolutionShareThem() {
        val indexes = TheoryIndexes()
        val context = PrologExecutionContext(startTime = 0, theoryIndexes = indexes)
        assertSame(indexes, indexesOf(context.createSolver()))
        assertSame(indexes, indexesOf(context.createMutableSolver()))
    }
}
