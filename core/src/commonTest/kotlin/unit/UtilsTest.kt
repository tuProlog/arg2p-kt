package unit

import it.unibo.argumentation.arg2p.Arg2p
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.ClassicSolverFactory
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.library.Libraries
import kotlin.test.Test
import kotlin.test.assertEquals

class UtilsTest {

    //TODO
    @Test
    fun assertaList() {
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
    fun sort() {
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
    fun subtractList() {
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
    fun isEmptyList() {
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
    fun appendaList() {
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
    fun search() {
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