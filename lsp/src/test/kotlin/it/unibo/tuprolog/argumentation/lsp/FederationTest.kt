package it.unibo.tuprolog.argumentation.lsp

import com.google.gson.JsonObject
import com.google.gson.JsonParser
import it.unibo.tuprolog.argumentation.Arg2pException
import java.net.ServerSocket
import java.net.URI
import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpResponse
import java.security.SecureRandom
import java.security.cert.X509Certificate
import java.util.concurrent.CompletableFuture
import java.util.concurrent.CyclicBarrier
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import javax.net.ssl.SSLContext
import javax.net.ssl.TrustManager
import javax.net.ssl.X509TrustManager
import kotlin.concurrent.thread
import kotlin.test.AfterTest
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertNull
import kotlin.test.assertTrue
import kotlin.time.Duration.Companion.seconds

/** The federation of the language server: its state, its pairings by code, its page, and the queries solved on it */
class FederationTest {
    private val tweety = """f1 :=> bird(tweety).\nr1 : bird(X) => flies(X).\nf2 :=> -flies(tweety).\n"""
    private val conflict = """r1 : [] => a.\nr2 : [] => -a.\nsup(r1, r2).\n"""
    private val flags = """argumentLabellingMode(grounded).\nstatementLabellingMode(statement).\nqueryMode.\n"""
    private val solving = Solving("test")
    private val federations = mutableListOf<Federation>()

    // A federation, listening on [port] once enabled (any free one for 0)
    private fun federation(
        port: Int = 0,
        changed: (FederationState) -> Unit = {},
    ) = Federation(solving, threads = 2, port = port, changed = changed).also { federations += it }

    // A port no one listens on
    private fun freePort() = ServerSocket(0).use { it.localPort }

    @AfterTest
    fun close() {
        federations.forEach { it.close() }
        solving.close()
    }

    @Test
    fun aFederationIsEnabledWithACodeAndALinkToJoinIt() {
        val changes = AtomicInteger()
        val port = freePort()
        val federation = federation(port) { changes.incrementAndGet() }
        assertEquals(false, federation.state.enabled)
        assertNull(federation.state.code)
        assertNull(federation.state.joinUrl)
        federation.enable(true)
        val state = federation.state
        assertTrue(state.enabled)
        assertTrue(Regex("""\d{4}""").matches(state.code!!))
        // a link on the local network, if the host has an address there
        state.joinUrl?.let { assertTrue(Regex("""https://\d+\.\d+\.\d+\.\d+:\d+/""").matches(it), it) }
        assertEquals(2, state.workers)
        federation.rename("desk")
        assertEquals("desk", federation.state.name)
        val renewed = (1..3).map { federation.newCode().let { federation.state.code } }
        assertTrue(renewed.any { it != state.code })
        assertTrue(changes.get() >= 5)
        federation.enable(false)
        assertNull(federation.state.code)
        assertNull(federation.state.joinUrl)
        assertEquals("desk", federation.state.name)
    }

    @Test
    fun federationsArePairedByTheAddressAndTheCodeOfTheOther() {
        val port = freePort()
        val first = federation().apply { enable(true) }
        val second = federation(port).apply { enable(true) }
        first.rename("first")
        second.rename("second")
        val url = "ws://127.0.0.1:$port"
        val wrong = if (second.state.code == "0000") "0001" else "0000"
        assertFailsWith<Arg2pException> { first.connect(url, wrong) }
        assertEquals(emptyList(), first.state.sites)
        first.connect(url, second.state.code!!)
        awaitUntil { second.state.sites.isNotEmpty() }
        assertEquals(
            "second",
            first.state.sites
                .single()
                .name,
        )
        assertEquals(
            "first",
            second.state.sites
                .single()
                .name,
        )
        assertFailsWith<Arg2pException> { first.connect("ws://127.0.0.1:1", "1234") }
    }

    @Test
    fun theJoinPageIsServedOnThePortOfTheFederation() {
        val port = freePort()
        federation(port).enable(true)
        val client = HttpClient.newHttpClient()
        val get = { path: String ->
            client.send(
                HttpRequest.newBuilder(URI("http://127.0.0.1:$port$path")).build(),
                HttpResponse.BodyHandlers.ofString(),
            )
        }
        val page = get("/")
        assertEquals(200, page.statusCode())
        assertEquals("text/html; charset=utf-8", page.headers().firstValue("Content-Type").get())
        assertTrue(Federation.MODULE in page.body())
        assertTrue("WebWorkerSolver" in page.body() && "connect(editor, code)" in page.body())
        assertEquals(200, get("/?from=qr").statusCode())
        val worker = get("/worker.js")
        assertEquals(200, worker.statusCode())
        assertTrue(Federation.MODULE in worker.body() && "startWebWorker(nodeService)" in worker.body())
        assertEquals(404, get("/other").statusCode())
    }

    // The page a phone joins from: over TLS, with a certificate of its own, and isolated across origins, for its Web Workers
    // to share memory
    @Test
    fun theJoinPageIsServedOverTlsIsolated() {
        val federation = federation(freePort())
        federation.enable(true)
        val url = federation.state.joinUrl ?: return
        val trusting =
            arrayOf<TrustManager>(
                object : X509TrustManager {
                    override fun checkClientTrusted(
                        chain: Array<X509Certificate>,
                        type: String,
                    ) = Unit

                    override fun checkServerTrusted(
                        chain: Array<X509Certificate>,
                        type: String,
                    ) = Unit

                    override fun getAcceptedIssuers(): Array<X509Certificate> = emptyArray()
                },
            )
        val ssl = SSLContext.getInstance("TLS").apply { init(null, trusting, SecureRandom()) }
        val client = HttpClient.newBuilder().sslContext(ssl).build()
        val page = client.send(HttpRequest.newBuilder(URI(url)).build(), HttpResponse.BodyHandlers.ofString())
        assertEquals(200, page.statusCode())
        assertEquals("same-origin", page.headers().firstValue("Cross-Origin-Opener-Policy").get())
        assertEquals("require-corp", page.headers().firstValue("Cross-Origin-Embedder-Policy").get())
        assertTrue("WebWorkerSolver" in page.body())
        val certificate = (
            page
                .sslSession()
                .get()
                .peerCertificates
                .single() as X509Certificate
        )
        certificate.verify(certificate.publicKey)
        certificate.checkValidity()
        val address = URI(url).host
        assertTrue(certificate.subjectAlternativeNames.any { it[1] == address }, "the certificate names $address")
    }

    @Test
    fun aParallelQueryHasTheSolutionsOfTheSolverOfThePage() {
        val engine = PageEngine(solving, parallel = federation())
        try {
            listOf("solve(In, Out, Und)", "solve(flies(tweety), In, Out, Und)").forEach { query ->
                assertEquals(solutions(engine, tweety, query, parallel = false), solutions(engine, tweety, query, parallel = true), query)
            }
            // a Prolog goal of the theory: the parallel solver tells the graph it last built with it
            assertEquals(
                solutions(engine, tweety, "bird(X)", parallel = false, graphs = false),
                solutions(engine, tweety, "bird(X)", parallel = true, graphs = false),
            )
        } finally {
            engine.close()
        }
    }

    @Test
    fun theParallelQueriesOfPagesOnOtherTheoriesDoNotMix() {
        val engine = PageEngine(solving, parallel = federation())
        try {
            ask(engine, solve(tweety, "solve", parallel = true), "one")
            ask(engine, solve(conflict, "solve", parallel = true), "two")
            val conclusions = { solution: JsonObject ->
                solution
                    .getAsJsonObject("graph")
                    .getAsJsonArray("arguments")
                    .map { it.asJsonObject.get("conclusion").asString }
                    .toSet()
            }
            val flies = solution(ask(engine, """{"type":"next","id":1}""", "one"))
            assertEquals(setOf("bird(tweety)", "flies(tweety)", "-flies(tweety)"), conclusions(flies))
            val a = solution(ask(engine, """{"type":"next","id":1}""", "two"))
            assertEquals(setOf("a", "-a"), conclusions(a))
            // the graph of the solution, again, as the only stage the parallel solver keeps
            val graph = ask(engine, """{"type":"graphAt","id":1,"branch":0,"reqId":2}""", "two").single().getAsJsonObject("graph")
            assertEquals(a.getAsJsonObject("graph"), graph)
            assertEquals(0, a.get("branch").asInt)
        } finally {
            engine.close()
        }
    }

    @Test
    fun aParallelQueryIsStopped() {
        val engine = PageEngine(solving, parallel = federation())
        try {
            val slow = """slow :- between(1, 100000000, _), fail.\nslow.\n"""
            val started = engine.handle("page", JsonParser.parseString(solve(slow, "slow", parallel = true)).asJsonObject)
            Thread.sleep(1_000)
            ask(engine, """{"type":"stop"}""")
            val stopped = answers(started).single { it.get("type").asString == "failed" }
            assertEquals(Resolution.STOPPED, stopped.getAsJsonObject("error").get("message").asString)
            // the next query solved as before
            assertEquals(solutions(engine, conflict, "a", parallel = false), solutions(engine, conflict, "a", parallel = true))
        } finally {
            engine.close()
        }
    }

    @Test
    fun aParallelQueryPastItsLimitFailsAsOutOfTime() {
        val engine = PageEngine(solving, parallel = federation())
        try {
            val slow = """slow :- between(1, 100000000, _), fail.\nslow.\n"""
            val request = JsonParser.parseString(solve(slow, "slow", parallel = true)).asJsonObject
            val failed = answers(engine.handle("page", request, 1.seconds)).single { it.get("type").asString == "failed" }
            assertEquals(TimedOut(1.seconds).message, failed.getAsJsonObject("error").get("message").asString)
            assertEquals(solutions(engine, conflict, "a", parallel = false), solutions(engine, conflict, "a", parallel = true))
        } finally {
            engine.close()
        }
    }

    @Test
    fun aFederationEnabledByManyAtOnceIsEnabledOnce() {
        val changes = AtomicInteger()
        val federation = federation { changes.incrementAndGet() }
        val engine = PageEngine(solving, parallel = federation)
        try {
            // a query solved on the parallel solver, which holds it: the calls enabling it wait for it together
            val slow = """slow :- between(1, 100000000, _), fail.
slow.
"""
            val solved = engine.handle("page", JsonParser.parseString(solve(slow, "slow", parallel = true)).asJsonObject)
            Thread.sleep(1_000)
            val ready = CyclicBarrier(THREADS)
            val enabling =
                List(THREADS) {
                    thread {
                        ready.await()
                        federation.enable(true)
                    }
                }
            enabling.forEach { it.join(30_000) }
            // the query stopped by the first of them
            answers(solved)
            assertTrue(federation.state.enabled)
            // one parallel solver made, told once
            assertEquals(1, changes.get())
            val code = federation.state.code
            federation.enable(true)
            assertEquals(code, federation.state.code)
            assertEquals(1, changes.get())
        } finally {
            engine.close()
        }
    }

    @Test
    fun aWrongParallelQueryTellsWhereItDoesNotParse() {
        val engine = PageEngine(solving, parallel = federation())
        try {
            val error = ask(engine, solve(tweety, "flies(", parallel = true)).single().getAsJsonObject("error")
            assertEquals("Query error", error.get("name").asString)
            assertEquals("query", error.getAsJsonObject("syntax").get("where").asString)
            val theory = ask(engine, solve("a :=> b(.", "a", parallel = true)).single().getAsJsonObject("error")
            assertEquals("Theory error", theory.get("name").asString)
        } finally {
            engine.close()
        }
    }

    private fun solve(
        theory: String,
        query: String,
        parallel: Boolean,
    ) = """{"type":"solve","id":1,"theory":"$theory","query":"$query","flags":"$flags","parallel":$parallel}"""

    // Every solution of [query] on [theory]: its result, its bindings, and the labels of the arguments of its graph if
    // [graphs]
    private fun solutions(
        engine: PageEngine,
        theory: String,
        query: String,
        parallel: Boolean,
        graphs: Boolean = true,
    ): List<Any> {
        assertEquals("started", ask(engine, solve(theory, query, parallel)).last().get("type").asString)
        val found = mutableListOf<Any>()
        do {
            val answer = ask(engine, """{"type":"next","id":1}""").single { it.get("type").asString == "solution" }
            val solution = answer.getAsJsonObject("solution")
            val labels =
                solution
                    .getAsJsonObject("graph")
                    .getAsJsonArray("arguments")
                    .map { it.asJsonObject.let { argument -> argument.get("conclusion").asString to argument.get("label").asString } }
                    .toSet()
            found.add(listOf(solution.get("result").asString, solution.get("bindings"), labels.takeIf { graphs }))
        } while (answer.get("hasNext").asBoolean)
        return found
    }

    private fun solution(answers: List<JsonObject>) = answers.single { it.get("type").asString == "solution" }.getAsJsonObject("solution")

    private fun ask(
        engine: PageEngine,
        request: String,
        session: String = "page",
    ) = answers(engine.handle(session, JsonParser.parseString(request).asJsonObject))

    private fun answers(answers: CompletableFuture<List<PageMessage>>) =
        json.toJsonTree(answers.get(60, TimeUnit.SECONDS)).asJsonArray.map { it.asJsonObject }

    private fun awaitUntil(condition: () -> Boolean) {
        val deadline = System.currentTimeMillis() + 20_000
        while (!condition()) {
            check(System.currentTimeMillis() < deadline) { "Not in time" }
            Thread.sleep(50)
        }
    }

    private companion object {
        // How many enable the federation at once
        const val THREADS = 8
    }
}
