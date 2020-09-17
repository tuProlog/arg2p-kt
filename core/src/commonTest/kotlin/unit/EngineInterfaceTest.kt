package unit

import it.unibo.argumentation.arg2p.Arg2p
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.ClassicSolverFactory
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.library.Libraries
import kotlin.test.Test
import kotlin.test.assertEquals

class EngineInterfaceTest {

    //TODO
    @Test
    fun buildLabelSets() {

        val solver = ClassicSolverFactory.solverWithDefaultBuiltins(
            otherLibraries = Libraries(Arg2p)
        )
        val query = Struct.parse("argTuProlog")
        val solutions = solver.solve(query)
        assertEquals(
            listOf(Solution.Yes(query)),
            solutions.toList()
        )
    }

    //TODO
    @Test
    fun answerQuery() {

        val solver = ClassicSolverFactory.solverWithDefaultBuiltins(
            otherLibraries = Libraries(Arg2p)
        )
        val query = Struct.parse("argTuProlog")
        val solutions = solver.solve(query)
        assertEquals(
            listOf(Solution.Yes(query)),
            solutions.toList()
        )
    }
}