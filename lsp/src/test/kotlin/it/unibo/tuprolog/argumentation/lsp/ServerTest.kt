package it.unibo.tuprolog.argumentation.lsp

import com.google.gson.JsonObject
import com.google.gson.JsonParser
import org.eclipse.lsp4j.ClientCapabilities
import org.eclipse.lsp4j.DidChangeTextDocumentParams
import org.eclipse.lsp4j.DidOpenTextDocumentParams
import org.eclipse.lsp4j.InitializeParams
import org.eclipse.lsp4j.MessageActionItem
import org.eclipse.lsp4j.MessageParams
import org.eclipse.lsp4j.PublishDiagnosticsParams
import org.eclipse.lsp4j.ShowMessageRequestParams
import org.eclipse.lsp4j.TextDocumentContentChangeEvent
import org.eclipse.lsp4j.TextDocumentItem
import org.eclipse.lsp4j.VersionedTextDocumentIdentifier
import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException
import org.eclipse.lsp4j.jsonrpc.messages.ResponseErrorCode
import org.eclipse.lsp4j.services.LanguageServer
import java.io.PipedInputStream
import java.io.PipedOutputStream
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutionException
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.TimeUnit
import kotlin.random.Random
import kotlin.test.AfterTest
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertNotNull
import kotlin.test.assertNull
import kotlin.test.assertTrue

/** The server as an editor speaks to it: JSON-RPC over a pair of pipes */
class ServerTest {
    // what the editor asks: the protocol of language servers, and the requests of arg2p
    internal interface Remote :
        LanguageServer,
        Arg2pService

    // the editor, receiving the diagnostics and the states of the federation
    private class Editor : Arg2pClient {
        val diagnostics = LinkedBlockingQueue<PublishDiagnosticsParams>()
        val federation = LinkedBlockingQueue<FederationState>()

        override fun federationChanged(state: FederationState) {
            federation.put(state)
        }

        override fun publishDiagnostics(diagnostics: PublishDiagnosticsParams) {
            this.diagnostics.put(diagnostics)
        }

        override fun telemetryEvent(`object`: Any?) = Unit

        override fun showMessage(messageParams: MessageParams) = Unit

        override fun showMessageRequest(requestParams: ShowMessageRequestParams): CompletableFuture<MessageActionItem> =
            CompletableFuture.completedFuture(null)

        override fun logMessage(message: MessageParams) = Unit
    }

    private val toServer = PipedOutputStream()
    private val toEditor = PipedOutputStream()
    private val editor = Editor()
    private val server =
        launcherOf(Arg2pLanguageServer(Random.nextInt(50_000, 60_000)), PipedInputStream(toServer, BUFFER), toEditor).also {
            it.startListening()
        }
    private val remote: Remote =
        Launcher
            .Builder<Remote>()
            .setLocalService(editor)
            .setRemoteInterface(Remote::class.java)
            .setInput(PipedInputStream(toEditor, BUFFER))
            .setOutput(toServer)
            .create()
            .also { it.startListening() }
            .remoteProxy

    @AfterTest
    fun close() {
        toServer.close()
        toEditor.close()
    }

    private fun <T> CompletableFuture<T>.done(): T = get(60, TimeUnit.SECONDS)

    // The answers of the engine to [request] of the page [session]
    private fun engine(
        request: String,
        session: String = "panel",
    ): List<JsonObject> =
        remote
            .engine(EngineParams(session, JsonParser.parseString(request).asJsonObject))
            .done()
            .answers
            .map { it.asJsonObject }

    @Test
    fun theServerSaysWhatItIs() {
        val result = remote.initialize(InitializeParams().apply { capabilities = ClientCapabilities() }).done()
        assertEquals("arg2p", result.serverInfo.name)
        assertNotNull(result.capabilities.textDocumentSync)
    }

    @Test
    fun aTheoryIsCheckedAsItChanges() {
        remote.initialize(InitializeParams().apply { capabilities = ClientCapabilities() }).done()
        remote.textDocumentService.didOpen(
            DidOpenTextDocumentParams(TextDocumentItem("file:///t.arg2p", "arg2p", 1, "a :=> b.\nc :=> d(.")),
        )
        val opened = editor.diagnostics.poll(60, TimeUnit.SECONDS)!!
        val problem = opened.diagnostics.single()
        assertEquals(1, problem.range.start.line)
        assertEquals(8, problem.range.start.character)
        assertEquals("Unexpected token '.'; expected term", problem.message.left)
        remote.textDocumentService.didChange(
            DidChangeTextDocumentParams(
                VersionedTextDocumentIdentifier("file:///t.arg2p", 2),
                listOf(TextDocumentContentChangeEvent("a :=> b.")),
            ),
        )
        assertTrue(
            editor.diagnostics
                .poll(60, TimeUnit.SECONDS)!!
                .diagnostics
                .isEmpty(),
        )
    }

    @Test
    fun aTheoryIsCheckedOnceItsTextSettles() {
        remote.initialize(InitializeParams().apply { capabilities = ClientCapabilities() }).done()
        remote.textDocumentService.didOpen(DidOpenTextDocumentParams(TextDocumentItem("file:///s.arg2p", "arg2p", 1, "a :=> b(.")))
        remote.textDocumentService.didChange(
            DidChangeTextDocumentParams(
                VersionedTextDocumentIdentifier("file:///s.arg2p", 2),
                listOf(TextDocumentContentChangeEvent("a :=> b.")),
            ),
        )
        // the text checked is the last one
        assertTrue(
            editor.diagnostics
                .poll(60, TimeUnit.SECONDS)!!
                .diagnostics
                .isEmpty(),
        )
    }

    @Test
    fun aPageOfTheEditorSolvesThroughTheServer() {
        assertEquals("warm", engine("""{"type":"warmUp"}""").single().get("type").asString)
        val started =
            engine(
                """{"type":"solve","id":1,"theory":"f1 :=> a.\nf2 :=> -a.\n","query":"buildLabelSets(In, Out, Und)","flags":"queryMode."}""",
            )
        assertEquals("started", started.single().get("type").asString)
        val solution = engine("""{"type":"next","id":1}""").single().getAsJsonObject("solution")
        assertEquals("yes", solution.get("result").asString)
        assertEquals(listOf("In", "Out", "Und"), solution.getAsJsonArray("bindings").map { it.asJsonObject.get("name").asString })
        val branch = solution.get("branch").asInt
        val graph = engine("""{"type":"graphAt","id":1,"branch":$branch,"reqId":2}""").single()
        assertEquals(solution.get("graph"), graph.get("graph"))
        val checked = engine("""{"type":"check","reqId":3,"text":"a :=> b.\nc :=> d(.","kind":"theory"}""").single()
        assertEquals(2, checked.getAsJsonObject("problem").get("line").asInt)
        // a text that parses has a problem null, not missing: the page tells them apart
        assertTrue(engine("""{"type":"check","reqId":4,"text":"a.","kind":"theory"}""").single().get("problem").isJsonNull)
        val settings = engine("""{"type":"settings","reqId":5,"theory":"graphBuildMode(abstract).","flags":""}""").single()
        assertEquals("abstract", settings.getAsJsonObject("settings").getAsJsonArray("graphBuildMode")[0].asString)
        assertTrue(engine("""{"type":"close"}""").isEmpty())
        assertEquals("failed", engine("""{"type":"next","id":1}""").single().get("type").asString)
    }

    @Test
    fun whatAQueryPrintsReachesThePage() {
        engine("""{"type":"solve","id":1,"theory":"","query":"write(hello)","flags":""}""")
        val printed = engine("""{"type":"next","id":1}""").first()
        assertEquals("output", printed.get("type").asString)
        assertEquals(1, printed.get("id").asInt)
        assertTrue("hello" in printed.get("text").asString)
    }

    @Test
    fun aPageStopsItsQuery() {
        engine("""{"type":"solve","id":1,"theory":"loop :- loop.","query":"loop","flags":""}""")
        val endless = remote.engine(EngineParams("panel", JsonParser.parseString("""{"type":"next","id":1}""").asJsonObject))
        Thread.sleep(500)
        assertTrue(engine("""{"type":"stop"}""").isEmpty())
        val stopped =
            endless
                .done()
                .answers
                .single()
                .asJsonObject
        assertEquals("failed", stopped.get("type").asString)
        assertEquals(PageEngine.STOPPED, stopped.getAsJsonObject("error").get("message").asString)
    }

    @Test
    fun aRequestThatIsNotOneOfTheEngineIsAnError() {
        val failure =
            assertFailsWith<ExecutionException> {
                remote.engine(EngineParams("panel", JsonParser.parseString("""{"type":"jump"}""").asJsonObject)).done()
            }.cause
        assertEquals(ResponseErrorCode.InvalidParams.value, (failure as ResponseErrorException).responseError.code)
        assertFailsWith<ExecutionException> { remote.engine(EngineParams(null, JsonObject())).done() }
    }

    @Test
    fun theFederationIsEnabledByTheEditorAndToldAsItChanges() {
        val options = JsonParser.parseString("""{"federation":{"enabled":true,"name":"desk"}}""")
        remote.initialize(InitializeParams().apply { initializationOptions = options }).done()
        val enabled = generateSequence { editor.federation.poll(60, TimeUnit.SECONDS)!! }.first { it.enabled }
        assertEquals("desk", enabled.name)
        assertEquals(enabled.code, remote.federation().done().code)
        assertEquals("lab", remote.rename(json("""{"name":"lab"}""")).done().name)
        assertTrue(Regex("""\d{4}""").matches(remote.newCode().done().code!!))
        // a site not nearby refuses, as parameters that are not those of the request are invalid
        assertEquals(ResponseErrorCode.RequestFailed.value, failure { remote.pair(json("""{"site":"site-none","code":"1234"}""")) })
        assertEquals(ResponseErrorCode.InvalidParams.value, failure { remote.pair(json("""{"site":"site-none"}""")) })
        assertEquals(ResponseErrorCode.InvalidParams.value, failure { remote.pair(null) })
        assertEquals(ResponseErrorCode.InvalidParams.value, failure { remote.enable(json("""{"enabled":"yes"}""")) })
        val disabled = remote.enable(json("""{"enabled":false}""")).done()
        assertEquals(false, disabled.enabled)
        assertNull(disabled.code)
        remote.shutdown().done()
    }

    private fun json(text: String) = JsonParser.parseString(text).asJsonObject

    // The code of the error a request fails with
    private fun failure(request: () -> CompletableFuture<*>): Int =
        (assertFailsWith<ExecutionException> { request().done() }.cause as ResponseErrorException).responseError.code

    private companion object {
        const val BUFFER = 1 shl 20
    }
}
