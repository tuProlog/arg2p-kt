package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.parallel.cluster.LocalCluster
import it.unibo.tuprolog.argumentation.parallel.cluster.ThreadedCluster
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import kotlin.random.Random
import kotlin.test.AfterTest
import kotlin.test.Test
import kotlin.test.assertEquals

// Clusters of the same machine finding each other through their beacons, on a port of their own (not the one of
// the clusters running meanwhile, if any)
class DiscoveryTest {
    private val clusters = mutableListOf<ThreadedCluster>()
    private val beaconPort = Random.nextInt(20_000, 30_000)

    @Test
    fun clustersDiscoveringFindEachOther() {
        repeat(3) { clusters += ThreadedCluster(threads = 1).also { it.discover(beaconPort) } }
        clusters.forEach { awaitPeers(it, 2) }
        val theory = "f0 :=> a0.\nr1 : a0 => a1.\nr2 : a1 => a2.\ng0 :=> b0.\ns1 : b0 => -a2.\n"
        clusters[0].load(theory)
        val expected = LocalCluster().also { it.load(theory) }
        listOf("a2", "-a2").forEach { assertEquals(labels(expected.solve(it)), labels(clusters[0].solve(it))) }
    }

    @Test
    fun aClusterComingBackIsFoundAgain() {
        val staying = ThreadedCluster(threads = 1).also { clusters += it }
        staying.discover(beaconPort)
        ThreadedCluster(threads = 1).also {
            it.discover(beaconPort)
            awaitPeers(staying, 1)
            it.close()
        }
        awaitPeers(staying, 0)
        clusters += ThreadedCluster(threads = 1).also { it.discover(beaconPort) }
        awaitPeers(staying, 1)
    }

    private fun labels(response: EvaluationResponse) = listOf(response.inArgs, response.outArgs, response.undArgs).map { it.size }

    private fun awaitPeers(
        cluster: ThreadedCluster,
        count: Int,
    ) {
        val deadline = System.currentTimeMillis() + 20_000
        while (cluster.site.peerSites.size != count) {
            check(System.currentTimeMillis() < deadline) { "${cluster.site.peerSites.size} peers rather than $count" }
            Thread.sleep(20)
        }
    }

    @AfterTest
    fun close() {
        clusters.forEach { it.close() }
        clusters.clear()
    }
}
