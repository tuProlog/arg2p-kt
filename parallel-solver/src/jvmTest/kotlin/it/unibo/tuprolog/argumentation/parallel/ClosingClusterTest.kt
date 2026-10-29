package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.parallel.cluster.ThreadedCluster
import java.util.concurrent.CompletableFuture
import java.util.concurrent.TimeUnit
import kotlin.concurrent.thread
import kotlin.test.Test
import kotlin.test.assertFailsWith
import kotlin.test.assertIs

// A cluster closing while its callers wait: they are not left waiting
class ClosingClusterTest {
    @Test
    fun theEvaluationsWaitingFailWhenTheClusterCloses() {
        val cluster = ThreadedCluster(threads = 1)
        val failure = CompletableFuture<Throwable?>()
        // a goal with no end, its first solution waited for
        thread { failure.complete(runCatching { cluster.solver.solve("repeat, fail") }.exceptionOrNull()) }
        Thread.sleep(500)
        cluster.close()
        assertIs<Arg2pException>(failure.get(10, TimeUnit.SECONDS))
        assertFailsWith<Arg2pException> { cluster.solve("a") }
    }

    @Test
    fun aPairingWithNoSiteNearbyFails() {
        ThreadedCluster(threads = 1).use { assertFailsWith<Arg2pException> { it.pair("site-none", "1234") } }
    }
}
