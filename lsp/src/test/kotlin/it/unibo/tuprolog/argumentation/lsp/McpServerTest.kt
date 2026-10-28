package it.unibo.tuprolog.argumentation.lsp

import com.google.gson.JsonObject
import com.google.gson.JsonParser
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.util.Base64
import java.util.zip.Inflater
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

/** The MCP server as an assistant speaks to it: one JSON-RPC message a line */
class McpServerTest {
    // The answers of the server to [messages], by their id
    private fun session(vararg messages: String): Map<Int, JsonObject> {
        val output = ByteArrayOutputStream()
        McpServer(ByteArrayInputStream(messages.joinToString("\n").toByteArray()), output).run()
        return output
            .toString(Charsets.UTF_8)
            .lines()
            .filter { it.isNotBlank() }
            .map { JsonParser.parseString(it).asJsonObject }
            .associateBy { it.get("id").asInt }
    }

    private fun call(
        id: Int,
        tool: String,
        arguments: String,
    ) = """{"jsonrpc":"2.0","id":$id,"method":"tools/call","params":{"name":"$tool","arguments":$arguments}}"""

    private val tweety = """f1 :=> bird(tweety).\nr1 : bird(X) => flies(X).\nf2 :=> -flies(tweety)."""

    @Test
    fun theServerSaysWhatItIsAndItsTools() {
        val answers =
            session(
                """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test","version":"1"}}}""",
                """{"jsonrpc":"2.0","method":"notifications/initialized"}""",
                """{"jsonrpc":"2.0","id":2,"method":"tools/list"}""",
                """{"jsonrpc":"2.0","id":3,"method":"nothing"}""",
            )
        val initialized = answers.getValue(1).getAsJsonObject("result")
        assertEquals("2025-06-18", initialized.get("protocolVersion").asString)
        assertEquals("arg2p", initialized.getAsJsonObject("serverInfo").get("name").asString)
        val tools =
            answers
                .getValue(2)
                .getAsJsonObject("result")
                .getAsJsonArray("tools")
                .map { it.asJsonObject.get("name").asString }
        assertEquals(listOf("solve", "answer", "check", "settings", "explore", "engine"), tools)
        assertEquals(
            -32601,
            answers
                .getValue(3)
                .getAsJsonObject("error")
                .get("code")
                .asInt,
        )
        // two answers for three requests and a notification: none for the notification
        assertEquals(setOf(1, 2, 3), answers.keys)
    }

    @Test
    fun aQueryIsSolvedWithItsGraph() {
        val result = session(call(1, "solve", """{"theory":"$tweety"}""")).getValue(1).getAsJsonObject("result")
        assertFalse(result.get("isError").asBoolean)
        val solved = result.getAsJsonObject("structuredContent")
        assertEquals("aspic_plus", solved.get("profile").asString)
        val solution = solved.getAsJsonArray("solutions")[0].asJsonObject
        assertEquals("yes", solution.get("result").asString)
        assertEquals("[[flies(tweety)]]", solution.getAsJsonObject("bindings").get("Out").asString)
        val arguments = solved.getAsJsonObject("graph").getAsJsonArray("arguments").map { it.asJsonObject }
        assertTrue(arguments.any { it.get("conclusion").asString == "flies(tweety)" && it.get("label").asString == "out" })
        assertTrue(
            result
                .getAsJsonArray("content")[0]
                .asJsonObject
                .get("text")
                .asString
                .contains("flies(tweety)"),
        )
    }

    @Test
    fun theStatementsOfAQueryAreLabelled() {
        val result = session(call(1, "answer", """{"theory":"$tweety","query":"flies(tweety)"}""")).getValue(1).getAsJsonObject("result")
        val labels = result.getAsJsonObject("structuredContent")
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
                call(4, "solve", """{"theory":"loop :- loop.","query":"loop","timeoutSeconds":1}"""),
            )
        val checked = answers.getValue(1).getAsJsonObject("result").getAsJsonObject("structuredContent")
        assertFalse(checked.get("parses").asBoolean)
        assertEquals(2, checked.getAsJsonObject("problem").get("line").asInt)
        val failed = answers.getValue(2).getAsJsonObject("result")
        assertTrue(failed.get("isError").asBoolean)
        assertTrue(
            failed
                .getAsJsonArray("content")[0]
                .asJsonObject
                .get("text")
                .asString
                .contains("line 1"),
        )
        val settings = answers.getValue(3).getAsJsonObject("result").getAsJsonObject("structuredContent")
        assertEquals("abstract", settings.getAsJsonArray("graphBuildMode")[0].asString)
        val endless = answers.getValue(4).getAsJsonObject("result")
        assertTrue(endless.get("isError").asBoolean)
        assertTrue(
            endless
                .getAsJsonArray("content")[0]
                .asJsonObject
                .get("text")
                .asString
                .contains("1 seconds"),
        )
    }

    @Test
    fun theSolverIsAnMcpApp() {
        val answers =
            session(
                """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{}}}""",
                """{"jsonrpc":"2.0","id":2,"method":"tools/list"}""",
                """{"jsonrpc":"2.0","id":3,"method":"resources/list"}""",
                """{"jsonrpc":"2.0","id":4,"method":"resources/read","params":{"uri":"ui://arg2p/solver"}}""",
            )
        val capabilities = answers.getValue(1).getAsJsonObject("result").getAsJsonObject("capabilities")
        assertTrue(capabilities.getAsJsonObject("extensions").has("io.modelcontextprotocol/ui"))
        val tools =
            answers
                .getValue(2)
                .getAsJsonObject("result")
                .getAsJsonArray("tools")
                .map { it.asJsonObject }
        val explore = tools.first { it.get("name").asString == "explore" }.getAsJsonObject("_meta").getAsJsonObject("ui")
        assertEquals("ui://arg2p/solver", explore.get("resourceUri").asString)
        val engine = tools.first { it.get("name").asString == "engine" }.getAsJsonObject("_meta").getAsJsonObject("ui")
        assertEquals(listOf("app"), engine.getAsJsonArray("visibility").map { it.asString })
        val listed =
            answers
                .getValue(3)
                .getAsJsonObject("result")
                .getAsJsonArray("resources")[0]
                .asJsonObject
        assertEquals("text/html;profile=mcp-app", listed.get("mimeType").asString)
        val page =
            answers
                .getValue(4)
                .getAsJsonObject("result")
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
    fun aTheoryExploredOpensInThePlaygroundToo() {
        val result = session(call(1, "explore", """{"theory":"$tweety"}""")).getValue(1).getAsJsonObject("result")
        assertFalse(result.get("isError").asBoolean)
        val explored = result.getAsJsonObject("structuredContent")
        assertEquals("aspic_plus", explored.get("profile").asString)
        val link = explored.get("playground").asString
        assertTrue(link.startsWith(Playground.URL + "?s="))
        val state = JsonParser.parseString(unpacked(link.substringAfter("?s="))).asJsonArray
        assertEquals(2, state[0].asInt)
        assertEquals(tweety.replace("\\n", "\n"), state[2].asString)
        assertEquals(MCP_DEFAULT_QUERY, state[3].asString)
        val text =
            result
                .getAsJsonArray("content")[0]
                .asJsonObject
                .get("text")
                .asString
        assertTrue(text.contains(link))
        val refused = session(call(2, "explore", """{"theory":"a :=> b(."}""")).getValue(2).getAsJsonObject("result")
        assertTrue(refused.get("isError").asBoolean)
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
                .getAsJsonObject("result")
                .getAsJsonObject("structuredContent")
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

    // The JSON a link of the playground packs
    private fun unpacked(packed: String): String {
        val inflater = Inflater(true)
        inflater.setInput(Base64.getUrlDecoder().decode(packed))
        // a raw stream, with no header asking for its dictionary
        inflater.setDictionary(PLAYGROUND_DICTIONARY)
        val output = ByteArrayOutputStream()
        val buffer = ByteArray(4096)
        while (!inflater.finished()) output.write(buffer, 0, inflater.inflate(buffer))
        inflater.end()
        return output.toString(Charsets.UTF_8)
    }

    private companion object {
        const val MCP_DEFAULT_QUERY = "buildLabelSets(In, Out, Und)"
        val PLAYGROUND_DICTIONARY =
            listOf(
                "[2,\"",
                "buildLabelSets(In, Out, Und)",
                "graphBuildMode(abstract).\\n",
                "graphBuildMode(aba).\\n",
                "graphBuildMode(aspic).\\n",
                "argumentLabellingMode(grounded).\\n",
                "argumentLabellingMode(preferred).\\n",
                "statementLabellingMode(statement).\\n",
                "queryMode.\\n",
                "assumption(",
                "contrary(",
                " :=> ",
                " => ",
                " :- ",
                " -> ",
                " <-> ",
                "(X)",
                "(X, Y)",
                "\\n\\n",
            ).joinToString("").toByteArray(Charsets.UTF_8)
    }
}
