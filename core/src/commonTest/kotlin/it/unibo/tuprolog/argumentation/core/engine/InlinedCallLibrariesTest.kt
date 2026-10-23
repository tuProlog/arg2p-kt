package it.unibo.tuprolog.argumentation.core.engine

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.model.mining.graph
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertSame
import kotlin.test.assertTrue

// The libraries of a module call only apply to its goal: a resolution ending within one (failing in it) leaves the
// solver with its own libraries
class InlinedCallLibrariesTest {
    @Test
    fun failingWithinAModuleCall() {
        val solver = Arg2pSolverFactory.default()
        val libraries = solver.libraries
        assertTrue(solver.solve(Struct.parse("graphStore::argumentIn(0, X)", solver.operators)).toList().none { it.isYes })
        assertSame(libraries, solver.libraries)
    }

    // A solver mines the graph of a theory, then solves another (as the causality solver does)
    @Test
    fun anotherTheoryAfterMining() {
        val solver = Arg2pSolverFactory.default()
        val operators = solver.operators

        fun labelOf(theory: String): String {
            solver.resetStaticKb()
            solver.loadStaticKb(Theory.parse(theory, operators))
            val answer = solver.solve(Struct.parse("answerQuery(c, R)", solver.operators)).first()
            solver.graph()
            return answer.substitution.getByName("R").toString()
        }
        assertEquals("[in(c)]", labelOf("f1 :=> a. r1 : a => c."))
        assertEquals("[out(c)]", labelOf("f1 :=> a. r1 : a => c. f2 :-> -a."))
    }
}
