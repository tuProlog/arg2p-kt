package it.unibo.tuprolog.argumentation.federation

import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.federation.Federation
import it.unibo.tuprolog.argumentation.federation.StaticContent
import it.unibo.tuprolog.argumentation.parallel.ThreadedSolver
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import java.net.URI
import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpResponse
import kotlin.test.AfterTest
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertTrue

// Parallel solvers linked only once they present the code of the other
class PairingSolverTest {
    private val solvers = mutableListOf<ThreadedSolver>()
    private val theory = "f0 :=> a0.\nr1 : a0 => a1.\nr2 : a1 => a2.\ng0 :=> b0.\ns1 : b0 => -a2.\n"

    private fun parallel() = ThreadedSolver(threads = 1).also { solvers += it }

    private fun federated() = Federation(parallel())

    private fun wrongCode(federation: Federation) = if (federation.code == "0000") "0001" else "0000"

    private val Federation.id get() = site.id

    @Test
    fun solversArePairedByTheCodeOfTheOther() {
        val first = federated()
        val second = federated().also { it.requiresCode = true }
        val port = second.listen(0, "127.0.0.1")
        val url = "ws://127.0.0.1:$port"
        assertFailsWith<Arg2pException> { first.connect(url, wrongCode(second)) }
        assertEquals(emptySet(), second.peers)
        first.name = "first"
        first.connect(url, second.code)
        awaitUntil { second.peers == setOf(first.id) }
        assertEquals(setOf(second.id), first.peers)
        assertEquals(setOf(second.id), first.site.pairedSites)
        assertEquals(setOf(first.id), second.site.pairedSites)
        assertEquals(
            "first",
            second.stats.sites
                .single { it.id == first.id }
                .name,
        )
        // the graph built by both
        val solving = solvers.single { it.id == first.id }
        solving.load(theory)
        val expected = ThreadedSolver(threads = 1).also { it.load(theory) }
        listOf("a2", "-a2").forEach { assertEquals(labels(expected.solve(it)), labels(solving.solve(it))) }
        assertEquals(
            setOf(first.id, second.id),
            solving.evaluation!!
                .workers
                .map { it.site }
                .toSet(),
        )
    }

    // The code is renewed once a site pairs: the site paired links again with no code, the others are refused
    @Test
    fun aSitePairedLinksAgainWithNoCode() {
        val listening = federated().also { it.requiresCode = true }
        val url = "ws://127.0.0.1:${listening.listen(0, "127.0.0.1")}"
        val code = listening.code
        val paired = federated()
        paired.connect(url, code)
        awaitUntil { listening.peers.size == 1 }
        assertTrue(listening.code != code)
        paired.connect(url)
        assertFailsWith<Arg2pException> { federated().connect(url, code) }
        assertFailsWith<Arg2pException> { federated().connect(url) }
    }

    @Test
    fun wrongCodesAreRefusedForAWhile() {
        val listening = federated().also { it.requiresCode = true }
        val port = listening.listen(0, "127.0.0.1")
        repeat(3) { assertFailsWith<Arg2pException> { federated().connect("ws://127.0.0.1:$port", wrongCode(listening)) } }
        // the right code refused as well, from this address, for a while
        assertFailsWith<Arg2pException> { federated().connect("ws://127.0.0.1:$port", listening.code) }
        assertEquals(emptySet(), listening.peers)
        Thread.sleep(1_500)
        val admitted = federated().also { it.connect("ws://127.0.0.1:$port", listening.code) }
        awaitUntil { listening.peers == setOf(admitted.id) }
    }

    @Test
    fun aConnectionPresentsTheCode() {
        val listening = federated().also { it.requiresCode = true }
        val port = listening.listen(0, "127.0.0.1")
        val refused = federated()
        assertFailsWith<Arg2pException> { refused.connect("ws://127.0.0.1:$port") }
        assertEquals(emptySet(), listening.peers)
        assertEquals(emptySet(), refused.peers)
        val admitted = federated().also { it.connect("ws://127.0.0.1:$port", listening.code) }
        awaitUntil { listening.peers == setOf(admitted.id) && admitted.peers == setOf(listening.id) }
        // a parallel solver requiring no code admits any
        val open = federated()
        val openPort = open.listen(0, "127.0.0.1")
        val other = federated().also { it.connect("ws://127.0.0.1:$openPort") }
        awaitUntil { open.peers == setOf(other.id) }
    }

    @Test
    fun pagesAreServedOnThePortOfTheWebSockets() {
        val listening = federated()
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
        val other = federated().also { it.connect("ws://127.0.0.1:$port") }
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
        solvers.forEach { it.close() }
        solvers.clear()
    }
}
