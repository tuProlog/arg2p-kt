package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Arg2pException
import java.util.concurrent.ExecutionException
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertIs

// A stop ends the goal under way at once, and the solver answers the next one without waiting for it to finish
class StopTest {
    // slow needs a premise that runs for over ten seconds (a deterministic Prolog loop on integers that fit a word: beyond,
    // each value is kept by the program); c1 is quick
    private val theory =
        "spin(N) :- (N =< 0 -> true ; M is N - 1, spin(M)).\n" +
            "f0 :=> go.\nr0 : go, prolog(spin(250000000)) => slow.\nu0 :=> c0.\nu1 : c0 => c1.\n"

    // (at different moments: while the theory is read, or the premise runs)
    @Test
    fun aStopEndsTheGoalUnderWayAndTheNextIsAnswered() = listOf(0L, 100L, 500L, 1500L).forEach { stopsAfter(it) }

    private fun stopsAfter(millis: Long) {
        val callers = Executors.newSingleThreadExecutor()
        // (one part: the solver builds the graph itself, on the task the stop interrupts)
        ThreadedSolver(threads = 2, parts = 1).use { parallel ->
            try {
                parallel.load(theory)
                val asked = java.util.concurrent.CountDownLatch(1)
                val slow =
                    callers.submit<Any> {
                        asked.countDown()
                        parallel.solve("slow")
                    }
                asked.await()
                // (the evaluation asked of the node before the stop, whatever the moment)
                Thread.sleep(millis + 20)
                parallel.goals.stop()
                val stopped = assertIs<ExecutionException>(runCatching { slow.get(10, TimeUnit.SECONDS) }.exceptionOrNull())
                assertIs<Arg2pException>(stopped.cause)
                // (well before the loop would end)
                val next = callers.submit<List<String>> { parallel.solve("c1").inArgs.map { it.claim } }
                assertEquals(listOf("c1"), next.get(3, TimeUnit.SECONDS))
            } finally {
                callers.shutdownNow()
            }
        }
    }
}
