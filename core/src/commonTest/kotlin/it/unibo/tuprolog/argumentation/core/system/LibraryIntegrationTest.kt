package it.unibo.tuprolog.argumentation.core.system

import it.unibo.tuprolog.argumentation.core.arg2p
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
            otherLibraries = arg2p().to2pLibraries()
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
        val libraries = arg2p().to2pLibraries()
        val theory = arg2p().to2pLibraries().theory
        val libTheory = libraries.theory
        assertEquals(libTheory, theory)
    }
}
