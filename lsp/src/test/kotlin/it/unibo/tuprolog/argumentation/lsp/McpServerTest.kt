package it.unibo.tuprolog.argumentation.lsp

import com.google.gson.JsonObject
import com.google.gson.JsonParser
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.OutputStream
import java.io.PipedInputStream
import java.io.PipedOutputStream
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.TimeUnit
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNotNull
import kotlin.test.assertNull
import kotlin.test.assertTrue

/** The MCP server as an assistant speaks to it: one JSON-RPC message a line */
class McpServerTest {
    // The answers of the server to [messages], by their id, once the client closed its input
    private fun session(vararg messages: String): Map<Int, JsonObject> {
        val output = ByteArrayOutputStream()
        McpServer(ByteArrayInputStream(messages.joinToString("\n").toByteArray()), output).run()
        return output
            .toString(Charsets.UTF_8)
            .lines()
            .filter { it.isNotBlank() }
            .map { JsonParser.parseString(it).asJsonObject }
            .associateBy { it.get("id").takeUnless { id -> id.isJsonNull }?.asInt ?: 0 }
    }

    // A server running, its answers read as they come
    private class Live : AutoCloseable {
        private val client = PipedOutputStream()
        private val answers = LinkedBlockingQueue<JsonObject>()
        val server =
            McpServer(
                PipedInputStream(client, 1 shl 20),
                object : OutputStream() {
                    private val line = ByteArrayOutputStream()

                    override fun write(byte: Int) {
                        if (byte == '\n'.code) {
                            answers.put(JsonParser.parseString(line.toString(Charsets.UTF_8)).asJsonObject)
                            line.reset()
                        } else {
                            line.write(byte)
                        }
                    }
                },
            )
        private val running = Thread { server.run() }.apply { start() }

        fun send(message: String) {
            client.write((message + "\n").toByteArray())
            client.flush()
        }

        // The next answer, within [seconds]
        fun answer(seconds: Long = 60): JsonObject? = answers.poll(seconds, TimeUnit.SECONDS)

        // Whether the server ends solving within [seconds]
        fun idle(seconds: Long = 10): Boolean {
            val until = System.nanoTime() + TimeUnit.SECONDS.toNanos(seconds)
            while (server.busy > 0 && System.nanoTime() < until) Thread.sleep(50)
            return server.busy == 0
        }

        override fun close() {
            client.close()
            running.join(60_000)
        }
    }

    private fun call(
        id: Int,
        tool: String,
        arguments: String,
    ) = """{"jsonrpc":"2.0","id":$id,"method":"tools/call","params":{"name":"$tool","arguments":$arguments}}"""

    private fun ping(id: Int) = """{"jsonrpc":"2.0","id":$id,"method":"ping"}"""

    private fun initialize(capabilities: String) =
        """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":$capabilities}}"""

    private val apps = """{"extensions":{"io.modelcontextprotocol/ui":{"mimeTypes":["text/html;profile=mcp-app"]}}}"""

    private val tweety = """f1 :=> bird(tweety).\nr1 : bird(X) => flies(X).\nf2 :=> -flies(tweety)."""

    private fun JsonObject.result() = getAsJsonObject("result")

    private fun JsonObject.content() = result().getAsJsonObject("structuredContent")

    private fun JsonObject.text() =
        result()
            .getAsJsonArray("content")[0]
            .asJsonObject
            .get("text")
            .asString

    private fun JsonObject.isError() = result().get("isError").asBoolean

    private fun JsonObject.errorCode() = getAsJsonObject("error").get("code").asInt

    private fun JsonObject.tools() =
        result()
            .getAsJsonArray("tools")
            .map { it.asJsonObject }

    @Test
    fun theServerSaysWhatItIsAndItsTools() {
        val answers =
            session(
                """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test","version":"1"}}}""",
                """{"jsonrpc":"2.0","method":"notifications/initialized"}""",
                """{"jsonrpc":"2.0","id":2,"method":"tools/list"}""",
                """{"jsonrpc":"2.0","id":3,"method":"nothing"}""",
            )
        val initialized = answers.getValue(1).result()
        assertEquals("2025-06-18", initialized.get("protocolVersion").asString)
        assertEquals("arg2p", initialized.getAsJsonObject("serverInfo").get("name").asString)
        // a client that does not show MCP Apps is not shown the engine of their page, nor the page
        val tools = answers.getValue(2).tools()
        assertEquals(listOf("solve", "answer", "check", "settings", "explore"), tools.map { it.get("name").asString })
        assertFalse(tools.single { it.get("name").asString == "explore" }.has("_meta"))
        assertEquals(-32601, answers.getValue(3).errorCode())
        // three answers for three requests and a notification: none for the notification
        assertEquals(setOf(1, 2, 3), answers.keys)
    }

    @Test
    fun theSolverIsAnMcpAppForTheClientsShowingThem() {
        val answers =
            session(
                initialize(apps),
                """{"jsonrpc":"2.0","id":2,"method":"tools/list"}""",
                """{"jsonrpc":"2.0","id":3,"method":"resources/list"}""",
                """{"jsonrpc":"2.0","id":4,"method":"resources/read","params":{"uri":"ui://arg2p/solver"}}""",
            )
        val capabilities = answers.getValue(1).result().getAsJsonObject("capabilities")
        assertTrue(capabilities.getAsJsonObject("extensions").has("io.modelcontextprotocol/ui"))
        val tools = answers.getValue(2).tools()
        val explore = tools.first { it.get("name").asString == "explore" }.getAsJsonObject("_meta").getAsJsonObject("ui")
        assertEquals("ui://arg2p/solver", explore.get("resourceUri").asString)
        val engine = tools.first { it.get("name").asString == "engine" }.getAsJsonObject("_meta").getAsJsonObject("ui")
        assertEquals(listOf("app"), engine.getAsJsonArray("visibility").map { it.asString })
        val listed =
            answers
                .getValue(3)
                .result()
                .getAsJsonArray("resources")[0]
                .asJsonObject
        assertEquals("text/html;profile=mcp-app", listed.get("mimeType").asString)
        val page =
            answers
                .getValue(4)
                .result()
                .getAsJsonArray("contents")[0]
                .asJsonObject
        val html = page.get("text").asString
        assertTrue(html.contains("@tuprolog/arg2p-ui@${McpApp.UI_VERSION}?bundle"))
        assertFalse(html.contains("{{"))
        assertEquals(
            "https://esm.sh",
            page
                .getAsJsonObject("_meta")
                .getAsJsonObject("ui")
                .getAsJsonObject("csp")
                .getAsJsonArray("resourceDomains")[0]
                .asString,
        )
    }

    @Test
    fun aQueryIsSolvedWithItsGraph() {
        val answer = session(call(1, "solve", """{"theory":"$tweety"}""")).getValue(1)
        assertFalse(answer.isError())
        val solved = answer.content()
        assertEquals("aspic_plus", solved.get("profile").asString)
        val solution = solved.getAsJsonArray("solutions")[0].asJsonObject
        assertEquals("yes", solution.get("result").asString)
        val bindings = solution.getAsJsonArray("bindings").map { it.asJsonObject }
        assertEquals("[[flies(tweety)]]", bindings.single { it.get("name").asString == "Out" }.get("value").asString)
        val arguments = solved.getAsJsonObject("graph").getAsJsonArray("arguments").map { it.asJsonObject }
        assertTrue(arguments.any { it.get("conclusion").asString == "flies(tweety)" && it.get("label").asString == "out" })
        assertFalse(solved.get("timedOut").asBoolean)
        assertTrue(answer.text().contains("flies(tweety)"))
    }

    @Test
    fun theStatementsOfAQueryAreLabelled() {
        val labels = session(call(1, "answer", """{"theory":"$tweety","query":"flies(tweety)"}""")).getValue(1).content()
        assertEquals(listOf("flies(tweety)"), labels.getAsJsonArray("out").map { it.asString })
        assertTrue(labels.getAsJsonArray("in").isEmpty)
    }

    @Test
    fun aTheoryThatDoesNotParseIsAnErrorOfTheTool() {
        val answers =
            session(
                call(1, "check", """{"text":"a :=> b.\nc :=> d(."}"""),
                call(2, "solve", """{"theory":"a :=> b(."}"""),
                call(3, "settings", """{"theory":"graphBuildMode(abstract).\na -> b."}"""),
                call(4, "settings", """{"theory":"a :=> b(."}"""),
                call(5, "explore", """{"theory":"a :=> b(."}"""),
            )
        val checked = answers.getValue(1).content()
        assertFalse(checked.get("parses").asBoolean)
        assertEquals(2, checked.getAsJsonObject("problem").get("line").asInt)
        assertTrue(answers.getValue(2).isError())
        assertTrue(answers.getValue(2).text().endsWith("at 1:9"), answers.getValue(2).text())
        assertEquals(
            "abstract",
            answers
                .getValue(3)
                .content()
                .getAsJsonArray("graphBuildMode")[0]
                .asString,
        )
        assertTrue(answers.getValue(4).isError())
        assertTrue(answers.getValue(4).text().contains("line 1, column 9"), answers.getValue(4).text())
        assertTrue(answers.getValue(5).isError())
    }

    @Test
    fun argumentsAToolDoesNotTakeAreItsErrors() {
        val answers =
            session(
                // the theory where the text goes
                call(1, "check", """{"theory":"a :=> b(."}"""),
                call(2, "solve", """{"query":"a"}"""),
                call(3, "solve", """{"theory":"a.","timeoutSeconds":"thirty"}"""),
                call(4, "solve", """{"theory":"a.","maxSolutions":1.5}"""),
                call(5, "answer", """{"theory":"a."}"""),
                call(6, "check", """{"text":"a.","kind":"prolog"}"""),
                call(7, "solve", """{"theory":{"rules":[]}}"""),
                call(8, "engine", """{"session":"page","request":{"type":"jump"}}"""),
                ping(9),
            )
        (1..8).forEach { id -> assertTrue(answers.getValue(id).isError(), "$id: ${answers.getValue(id)}") }
        assertTrue(answers.getValue(1).text().contains("text is missing"))
        assertTrue(answers.getValue(3).text().contains("timeoutSeconds is not an integer"))
        assertNotNull(answers.getValue(9).result())
    }

    @Test
    fun aMessageThatIsNotARequestOfTheServerIsAnErrorOfJsonRpc() {
        val answers =
            session(
                """{"jsonrpc":"2.0","id":1,"method":"tools/call","params":"solve"}""",
                """{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"solve","arguments":"a."}}""",
                """{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":3}}""",
                call(4, "jump", "{}"),
                """{"jsonrpc":"2.0","id":5,"method":5}""",
                """{"jsonrpc":"2.0","id":6,"method":"resources/read","params":{"uri":"ui://arg2p/nothing"}}""",
                """{"jsonrpc":"2.0","id":7,"method":"resources/read"}""",
                ping(8),
            )
        assertEquals(listOf(-32602, -32602, -32602, -32602, -32600, -32002, -32602), (1..7).map { answers.getValue(it).errorCode() })
        assertNotNull(answers.getValue(8).result())
        assertEquals(-32700, session("not json").getValue(0).errorCode())
        assertEquals(-32600, session("[1, 2]").getValue(0).errorCode())
    }

    @Test
    fun aQueryOutOfTimeIsStopped() {
        Live().use { live ->
            live.send(call(1, "solve", """{"theory":"loop :- loop.","query":"loop","timeoutSeconds":1}"""))
            val endless = assertNotNull(live.answer())
            assertTrue(endless.isError())
            assertTrue(endless.text().contains("1 seconds"))
            // no thread is left solving it
            assertTrue(live.idle())
            live.send(ping(2))
            assertEquals(2, assertNotNull(live.answer(5)).get("id").asInt)
        }
    }

    @Test
    fun aTimeoutIsPositive() {
        val endless = session(call(1, "solve", """{"theory":"loop :- loop.","query":"loop","timeoutSeconds":-5}""")).getValue(1)
        assertTrue(endless.isError())
        assertTrue(endless.text().contains("1 seconds"))
    }

    @Test
    fun theSolutionsFoundInTimeAreGiven() {
        val theory = """a.\na :- loop.\nloop :- loop."""
        val answer = session(call(1, "solve", """{"theory":"$theory","query":"a","timeoutSeconds":2}""")).getValue(1)
        assertFalse(answer.isError())
        val found = answer.content()
        assertTrue(found.get("timedOut").asBoolean)
        assertEquals(listOf("yes"), found.getAsJsonArray("solutions").map { it.asJsonObject.get("result").asString })
    }

    @Test
    fun noSolutionIsLookedForBeyondThoseAskedFor() {
        val theory = """a.\na :- loop.\nloop :- loop."""
        val started = System.nanoTime()
        val answer = session(call(1, "solve", """{"theory":"$theory","query":"a","maxSolutions":1,"timeoutSeconds":30}""")).getValue(1)
        assertTrue(System.nanoTime() - started < TimeUnit.SECONDS.toNanos(20))
        val found = answer.content()
        assertFalse(found.get("timedOut").asBoolean)
        assertTrue(found.get("more").asBoolean)
        assertEquals(1, found.getAsJsonArray("solutions").size())
    }

    @Test
    fun theServerAnswersWhileATheoryIsSolved() {
        Live().use { live ->
            live.send(call(1, "solve", """{"theory":"loop :- loop.","query":"loop","timeoutSeconds":60}"""))
            live.send(ping(2))
            assertEquals(2, assertNotNull(live.answer(10)).get("id").asInt)
            // the client gives the query up: it stops, unanswered
            live.send("""{"jsonrpc":"2.0","method":"notifications/cancelled","params":{"requestId":1}}""")
            assertTrue(live.idle())
            assertNull(live.answer(1))
        }
    }

    @Test
    fun aTheoryExploredOpensInThePlaygroundToo() {
        val answer = session(call(1, "explore", """{"theory":"$tweety"}""")).getValue(1)
        assertFalse(answer.isError())
        val explored = answer.content()
        assertEquals("aspic_plus", explored.get("profile").asString)
        val link = explored.get("playground").asString
        assertTrue(link.startsWith(Playground.URL + "?s="))
        val state = JsonParser.parseString(Playground.state(link.substringAfter("?s="))).asJsonArray
        assertEquals(2, state[0].asInt)
        assertEquals(Solvers.DEFAULT_FLAGS, state[1].asString)
        assertEquals(tweety.replace("\\n", "\n"), state[2].asString)
        assertEquals(Solvers.DEFAULT_QUERY, state[3].asString)
        // shown in the conversation only to a client of MCP Apps
        assertTrue(answer.text().contains(link))
        assertFalse(answer.text().contains("conversation"))
        val shown = session(initialize(apps), call(2, "explore", """{"theory":"$tweety"}""")).getValue(2)
        assertTrue(shown.text().contains("conversation"))
    }

    @Test
    fun eachSolverShownSolvesOnTheEngineOfItsOwn() {
        fun engine(
            id: Int,
            session: String,
            request: String,
        ) = call(id, "engine", """{"session":"$session","request":$request}""")
        val answers =
            session(
                engine(1, "one", """{"type":"solve","id":1,"theory":"$tweety","query":"flies(tweety)","flags":"queryMode."}"""),
                engine(2, "two", """{"type":"solve","id":1,"theory":"$tweety","query":"bird(tweety)","flags":"queryMode."}"""),
                engine(3, "one", """{"type":"next","id":1}"""),
                engine(4, "two", """{"type":"next","id":1}"""),
                engine(5, "one", """{"type":"check","reqId":7,"text":"a :=> b(.","kind":"theory"}"""),
                engine(6, "three", """{"type":"next","id":1}"""),
            )

        fun answersOf(id: Int) =
            answers
                .getValue(id)
                .content()
                .getAsJsonArray("answers")
                .map { it.asJsonObject }
        assertEquals("started", answersOf(1).single().get("type").asString)
        val one = answersOf(3).single { it.get("type").asString == "solution" }
        assertEquals("flies(tweety)", one.getAsJsonObject("solution").get("query").asString)
        val two = answersOf(4).single { it.get("type").asString == "solution" }
        assertEquals("bird(tweety)", two.getAsJsonObject("solution").get("query").asString)
        val checked = answersOf(5).single()
        assertEquals(7, checked.get("reqId").asInt)
        assertEquals(1, checked.getAsJsonObject("problem").get("line").asInt)
        assertEquals("failed", answersOf(6).single().get("type").asString)
    }
}
