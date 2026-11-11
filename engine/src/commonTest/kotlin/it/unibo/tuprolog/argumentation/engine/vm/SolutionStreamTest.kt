package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.solverWithTheory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertTrue

/** The solutions of a goal stop when cancelled (a halt, the last) or closed (none more), however many were left */
class SolutionStreamTest {
    private val naturals = "nat(0).\nnat(N) :- nat(M), N is M + 1."

    @Test
    fun cancelled() {
        val solver = solverWithTheory(naturals)
        val solutions = solver.solve(Struct.parse("nat(X)", solver.operators))
        val iterator = solutions.iterator()
        assertTrue(iterator.next().isYes)
        solutions.cancel()
        assertTrue(solutions.isCancelled)
        // (the machine reads the cancellation within a few calls: some solutions may come before the halt)
        val rest = generateSequence { if (iterator.hasNext()) iterator.next() else null }.take(100_000).toList()
        assertTrue(rest.last().isHalt)
        assertTrue(rest.dropLast(1).all { it.isYes })
        assertFalse(iterator.hasNext())
    }

    @Test
    fun closed() {
        val solver = solverWithTheory(naturals)
        val solutions = solver.solve(Struct.parse("nat(X)", solver.operators))
        val iterator = solutions.iterator()
        assertTrue(iterator.next().isYes)
        solutions.close()
        assertFalse(iterator.hasNext())
        // (the solver solves again after it, its machine given back)
        assertTrue(solver.solve(Struct.parse("nat(2)", solver.operators)).first().isYes)
    }
}
