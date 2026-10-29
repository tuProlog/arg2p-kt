package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.parallel.cluster.LocalCluster
import it.unibo.tuprolog.argumentation.parallel.cluster.ThreadedCluster
import it.unibo.tuprolog.argumentation.parallel.federation.StaticContent
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import java.net.URI
import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpResponse
import kotlin.random.Random
import kotlin.test.AfterTest
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertTrue

// Clusters linked only once they present the code of the other: found on the local network, or connecting
class PairingClusterTest {
    private val clusters = mutableListOf<ThreadedCluster>()
    private val beaconPort = Random.nextInt(30_000, 40_000)
    private val theory = "f0 :=> a0.\nr1 : a0 => a1.\nr2 : a1 => a2.\ng0 :=> b0.\ns1 : b0 => -a2.\n"

    private fun cluster() = ThreadedCluster(threads = 1).also { clusters += it }

    private fun wrongCode(cluster: ThreadedCluster) = if (cluster.code == "0000") "0001" else "0000"

    @Test
    fun clustersRequiringACodeAreNearbyUntilPaired() {
        val (first, second) = List(2) { cluster().also { it.discover(beaconPort, requireCode = true) } }
        first.name = "first"
        awaitUntil { first.nearby.any { it.id == second.id } && second.nearby.any { it.id == first.id && it.name == "first" } }
        assertTrue(first.nearby.single { it.id == second.id }.requiresCode)
        // a few beacons more: still not linked
        Thread.sleep(3_000)
        assertEquals(emptySet(), first.peers)
        assertFailsWith<Arg2pException> { first.pair(second.id, wrongCode(second)) }
        assertEquals(emptySet(), second.peers)
        first.pair(second.id, second.code)
        awaitUntil { second.peers == setOf(first.id) }
        assertEquals(setOf(second.id), first.peers)
        assertEquals(setOf(second.id), first.site.pairedSites)
        assertEquals(setOf(first.id), second.site.pairedSites)
        assertTrue(first.nearby.none { it.id == second.id })
        // the graph built by both
        first.load(theory)
        val expected = LocalCluster().also { it.load(theory) }
        listOf("a2", "-a2").forEach { assertEquals(labels(expected.solve(it)), labels(first.solve(it))) }
        assertEquals(
            setOf(first.id, second.id),
            first.stats.evaluation!!
                .workers
                .map { it.site }
                .toSet(),
        )
    }

    @Test
    fun wrongCodesAreRefusedForAWhile() {
        val listening = cluster().also { it.requiresCode = true }
        val port = listening.listen(0, "127.0.0.1")
        repeat(3) { cluster().connect("ws://127.0.0.1:$port", wrongCode(listening)) }
        // the right code refused as well, from this address, for a while
        cluster().connect("ws://127.0.0.1:$port", listening.code)
        Thread.sleep(500)
        assertEquals(emptySet(), listening.peers)
        Thread.sleep(1_500)
        val admitted = cluster().also { it.connect("ws://127.0.0.1:$port", listening.code) }
        awaitUntil { listening.peers == setOf(admitted.id) }
    }

    @Test
    fun aConnectionPresentsTheCode() {
        val listening = cluster().also { it.requiresCode = true }
        val port = listening.listen(0, "127.0.0.1")
        val refused = cluster().also { it.connect("ws://127.0.0.1:$port") }
        Thread.sleep(500)
        assertEquals(emptySet(), listening.peers)
        assertEquals(emptySet(), refused.peers)
        val admitted = cluster().also { it.connect("ws://127.0.0.1:$port", listening.code) }
        awaitUntil { listening.peers == setOf(admitted.id) && admitted.peers == setOf(listening.id) }
        // a cluster requiring no code admits any
        val open = cluster()
        val openPort = open.listen(0, "127.0.0.1")
        val other = cluster().also { it.connect("ws://127.0.0.1:$openPort") }
        awaitUntil { open.peers == setOf(other.id) }
    }

    @Test
    fun pagesAreServedOnThePortOfTheWebSockets() {
        val listening = cluster()
        listening.serve { path ->
            if (path == "/join") StaticContent("<p>join</p>".encodeToByteArray(), "text/html; charset=utf-8") else null
        }
        val port = listening.listen(0, "127.0.0.1")
        val client = HttpClient.newHttpClient()
        val get = { path: String ->
            client.send(HttpRequest.newBuilder(URI("http://127.0.0.1:$port$path")).build(), HttpResponse.BodyHandlers.ofString())
        }
        val page = get("/join")
        assertEquals(200, page.statusCode())
        assertEquals("<p>join</p>", page.body())
        assertEquals("text/html; charset=utf-8", page.headers().firstValue("Content-Type").get())
        assertEquals(404, get("/other").statusCode())
        // the WebSockets as before
        val other = cluster().also { it.connect("ws://127.0.0.1:$port") }
        awaitUntil { listening.peers == setOf(other.id) }
    }

    private fun labels(response: EvaluationResponse) = listOf(response.inArgs, response.outArgs, response.undArgs).map { it.size }

    private fun awaitUntil(condition: () -> Boolean) {
        val deadline = System.currentTimeMillis() + 20_000
        while (!condition()) {
            check(System.currentTimeMillis() < deadline) { "Not in time" }
            Thread.sleep(20)
        }
    }

    @AfterTest
    fun close() {
        clusters.forEach { it.close() }
        clusters.clear()
    }
}
