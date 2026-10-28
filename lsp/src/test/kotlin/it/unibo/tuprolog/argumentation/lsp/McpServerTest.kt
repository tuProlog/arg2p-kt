package it.unibo.tuprolog.argumentation.lsp

import com.google.gson.JsonObject
import com.google.gson.JsonParser
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
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
        assertEquals(listOf("solve", "answer", "check", "settings"), tools)
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
}
