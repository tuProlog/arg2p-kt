package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.solverWithTheory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

/** The solutions of a goal are those of one resolution: iterated once, a second iteration refused rather than wrong */
class SolutionSequenceTest {
    @Test
    fun iteratedOnce() {
        val solver = solverWithTheory("p(1). p(2).")
        val solutions = solver.solve(Struct.parse("p(X)", solver.operators))
        assertEquals(2, solutions.toList().size)
        assertFailsWith<IllegalStateException> { solutions.toList() }
    }
}
