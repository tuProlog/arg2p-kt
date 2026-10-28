package it.unibo.tuprolog.argumentation.lsp

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
import org.eclipse.lsp4j.services.LanguageServer
import java.io.PipedInputStream
import java.io.PipedOutputStream
import java.util.concurrent.CompletableFuture
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.TimeUnit
import kotlin.test.AfterTest
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotNull
import kotlin.test.assertTrue

/** The server as an editor speaks to it: JSON-RPC over a pair of pipes */
class ServerTest {
    // what the editor asks: the protocol of language servers, and the requests of arg2p
    interface Remote :
        LanguageServer,
        Arg2pService

    // the editor, receiving the diagnostics and what runs print
    private class Editor : Arg2pClient {
        val diagnostics = LinkedBlockingQueue<PublishDiagnosticsParams>()
        val printed = LinkedBlockingQueue<OutputParams>()

        override fun publishDiagnostics(diagnostics: PublishDiagnosticsParams) {
            this.diagnostics.put(diagnostics)
        }

        override fun output(params: OutputParams) = printed.put(params)

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
        launcherOf(Arg2pLanguageServer(), PipedInputStream(toServer, BUFFER), toEditor).also { it.startListening() }
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
    fun anEditorSolvesThroughTheServer() {
        val started = remote.solve(SolveParams("f1 :=> a.\nf2 :=> -a.\n", "buildLabelSets(In, Out, Und)", "queryMode.")).done()
        val answer = remote.next(RunParams(started.id)).done()
        assertEquals("yes", answer.solution?.result)
        assertEquals(listOf("In", "Out", "Und"), answer.solution?.bindings?.map { it.name })
        assertEquals(answer.solution?.graph, remote.graphAt(GraphAtParams(started.id, answer.solution!!.branch)).done())
        assertEquals(2, remote.check(CheckParams("a :=> b.\nc :=> d(.", "theory")).done()?.line)
        assertEquals(listOf("abstract"), remote.settings(SettingsParams("graphBuildMode(abstract).", "")).done()["graphBuildMode"])
    }

    @Test
    fun whatARunPrintsReachesTheEditor() {
        val started = remote.solve(SolveParams("", "write(hello)", "")).done()
        remote.next(RunParams(started.id)).done()
        val printed = editor.printed.poll(60, TimeUnit.SECONDS)!!
        assertEquals(started.id, printed.id)
        assertTrue("hello" in printed.text)
    }

    private companion object {
        const val BUFFER = 1 shl 20
    }
}
