package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.argumentation.parallel.cluster.ThreadedCluster
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import kotlin.test.AfterTest
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

// The scenarios on three clusters federated over WebSockets on this machine: the two lending their workers connect
// to the evaluating one, and talk to each other through it
class WebSocketClusterTest : EquivalenceScenarios() {
    private val clusters = mutableListOf<ThreadedCluster>()

    override fun distributed(
        theory: String,
        settings: FlagsBuilder,
    ): (String) -> EvaluationResponse {
        val evaluating = ThreadedCluster(threads = 1, settings = settings).also { clusters += it }
        val port = evaluating.listen(0, "127.0.0.1")
        repeat(2) {
            ThreadedCluster(threads = 2).also {
                clusters += it
                it.connect("ws://127.0.0.1:$port")
            }
        }
        awaitPeers(evaluating, 2)
        evaluating.load(theory)
        return evaluating::solve
    }

    @Test
    fun aClusterLeavingIsNoLongerAPeer() {
        val evaluating = ThreadedCluster(threads = 1).also { clusters += it }
        val port = evaluating.listen(0, "127.0.0.1")
        val leaving = ThreadedCluster(threads = 1).also { it.connect("ws://127.0.0.1:$port") }
        awaitPeers(evaluating, 1)
        leaving.close()
        awaitPeers(evaluating, 0)
        assertEquals(emptySet(), evaluating.site.peerSites)
    }

    @Test
    fun connectingWhereNoSiteListensFails() {
        val cluster = ThreadedCluster(threads = 1).also { clusters += it }
        assertFailsWith<IllegalStateException> { cluster.connect("ws://127.0.0.1:1") }
    }

    // The links greet each other a moment after they open
    private fun awaitPeers(
        cluster: ThreadedCluster,
        count: Int,
    ) {
        val deadline = System.currentTimeMillis() + 10_000
        while (cluster.site.peerSites.size != count) {
            check(System.currentTimeMillis() < deadline) { "${cluster.site.peerSites.size} peers rather than $count" }
            Thread.sleep(5)
        }
    }

    @AfterTest
    fun close() {
        clusters.forEach { it.close() }
        clusters.clear()
    }
}
