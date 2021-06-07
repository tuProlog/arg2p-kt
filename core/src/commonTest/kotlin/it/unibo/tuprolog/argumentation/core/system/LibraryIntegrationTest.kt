package it.unibo.tuprolog.argumentation.core.system

import it.unibo.tuprolog.argumentation.core.Arg2p
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.classic.ClassicSolverFactory
import it.unibo.tuprolog.solve.library.Libraries
import kotlin.test.Test
import kotlin.test.assertEquals

class LibraryIntegrationTest {

    @Test
    fun libraryLoading() {
        val solver = ClassicSolverFactory.solverWithDefaultBuiltins(
            otherLibraries = Libraries.of(Arg2p)
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
        val libraries = Libraries.of(Arg2p)
        val theory = Arg2p.theory
        val libTheory = libraries.theory
        assertEquals(libTheory, theory)
    }
}
