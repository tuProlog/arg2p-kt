package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.Arg2pPrologSolverFactory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import kotlin.test.Test
import kotlin.test.assertTrue

// A stop counted on shared memory halts the resolutions of the thread watching it, until the thread takes it up
class InterruptionTest {
    @Test
    fun aStopCountedHaltsTheResolutionUntilTakenUp() {
        val counter: dynamic = js("new Int32Array(new SharedArrayBuffer(4))")
        watchStops(counter, 0)
        val solver =
            Arg2pPrologSolverFactory.solverWithDefaultBuiltins(
                staticKb = Theory.parse("spin(N) :- (N =< 0 -> true ; M is N - 1, spin(M))."),
            )
        assertTrue(solver.solveOnce(Struct.parse("spin(100000)")).isYes)
        js("Atomics").add(counter, 0, 1)
        // (a loop of minutes, halted at the first check)
        assertTrue(solver.solveOnce(Struct.parse("spin(250000000)")).isHalt)
        stopTakenUp()
        assertTrue(solver.solveOnce(Struct.parse("spin(100000)")).isYes)
    }
}
