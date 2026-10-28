package it.unibo.tuprolog.argumentation

import java.util.concurrent.CompletableFuture
import java.util.concurrent.TimeUnit
import kotlin.test.Test
import kotlin.test.assertTrue

/** A resolution stops when its thread is interrupted: how a server stops a query that does not end */
class InterruptionTest {
    @Test
    fun aResolutionStopsOnceItsThreadIsInterrupted() {
        val answer = CompletableFuture<Solution>()
        val thread = Thread(null, { answer.complete(Arg2p.of("loop :- loop.").solve("loop").next()) }, "loop", 512L * 1024 * 1024)
        thread.start()
        Thread.sleep(500)
        thread.interrupt()
        val solution = answer.get(30, TimeUnit.SECONDS)
        assertTrue(solution.isHalt)
    }
}
