package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.MachineTesting
import kotlin.test.Test
import kotlin.test.assertTrue

// The numbers no word holds are collected with the heap (see Numbers): a loop over millions of them keeps none
class LargeNumbersMemoryTest {
    private fun used(): Long {
        repeat(3) { System.gc() }
        val runtime = Runtime.getRuntime()
        return runtime.totalMemory() - runtime.freeMemory()
    }

    @Test
    fun aLoopOverLargeIntegersAndFloatsKeepsNone() {
        val machine =
            MachineTesting(
                "spin(N, E) :- (N =< E -> true ; M is N - 1, spin(M, E)).\n" +
                    "drift(N, X) :- (N =< 0 -> true ; Y is X + 0.5, M is N - 1, drift(M, Y)).\n",
            )
        assertTrue(machine.holds("spin(3000100000, 3000000000)"))
        assertTrue(machine.holds("drift(100000, 0.25)"))
        val before = used()
        assertTrue(machine.holds("spin(3005000000, 3000000000)"))
        assertTrue(machine.holds("drift(1000000, 0.25)"))
        val grown = (used() - before) / 1048576
        assertTrue(grown < 32, "the heap grew by $grown MB over five million large integers and a million floats")
    }
}
