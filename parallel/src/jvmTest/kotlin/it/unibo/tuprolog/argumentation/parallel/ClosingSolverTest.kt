package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.parallel.ThreadedSolver
import java.util.concurrent.CompletableFuture
import java.util.concurrent.TimeUnit
import kotlin.concurrent.thread
import kotlin.test.Test
import kotlin.test.assertFailsWith
import kotlin.test.assertIs

// A parallel solver closing while its callers wait: they are not left waiting
class ClosingSolverTest {
    @Test
    fun theEvaluationsWaitingFailWhenTheSolverCloses() {
        val parallel = ThreadedSolver(threads = 1)
        val failure = CompletableFuture<Throwable?>()
        // a goal with no end, its first solution waited for
        thread { failure.complete(runCatching { parallel.goals.solve("repeat, fail") }.exceptionOrNull()) }
        Thread.sleep(500)
        parallel.close()
        assertIs<Arg2pException>(failure.get(10, TimeUnit.SECONDS))
        assertFailsWith<Arg2pException> { parallel.solve("a") }
    }
}
