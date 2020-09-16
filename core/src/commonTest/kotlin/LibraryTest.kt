import it.unibo.argumentation.arg2p.Arg2p
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.ClassicSolverFactory
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.library.Libraries
import kotlin.test.Test
import kotlin.test.assertEquals

class LibraryTest {

    @Test
    fun loadLibraries() {
        val solver = ClassicSolverFactory.solverWithDefaultBuiltins(
            otherLibraries = Libraries(Arg2p)
        )
        val query = Struct.parse("query_here")
        val solutions = solver.solve(query)
        assertEquals(
            listOf(Solution.No(query)),
            solutions.toList()
        )
    }
}