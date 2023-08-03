package it.unibo.tuprolog.argumentation.core.system

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.classic.ClassicSolverFactory
import kotlin.test.Test
import kotlin.test.assertEquals

class LibraryIntegrationTest {

    @Test
    fun libraryLoading() {
        val solver = ClassicSolverFactory.solverWithDefaultBuiltins(
            otherLibraries = Arg2pSolver.default().to2pLibraries()
        )
        val query = Struct.parse("argTuProlog")
        val solutions = solver.solve(query)
        assertEquals(
            listOf(Solution.yes(query)),
            solutions.toList()
        )
    }

    @Test
    fun library() {
        val libraries = Arg2pSolver.default().to2pLibraries()
        val theory = Arg2pSolver.default().to2pLibraries().clauses
        val libTheory = libraries.clauses
        assertEquals(libTheory, theory)
    }
}
