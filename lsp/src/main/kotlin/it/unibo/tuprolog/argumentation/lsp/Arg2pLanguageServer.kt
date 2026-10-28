package it.unibo.tuprolog.argumentation.lsp

import it.unibo.tuprolog.argumentation.Arg2p
import org.eclipse.lsp4j.Diagnostic
import org.eclipse.lsp4j.DiagnosticSeverity
import org.eclipse.lsp4j.DidChangeConfigurationParams
import org.eclipse.lsp4j.DidChangeTextDocumentParams
import org.eclipse.lsp4j.DidChangeWatchedFilesParams
import org.eclipse.lsp4j.DidCloseTextDocumentParams
import org.eclipse.lsp4j.DidOpenTextDocumentParams
import org.eclipse.lsp4j.DidSaveTextDocumentParams
import org.eclipse.lsp4j.InitializeParams
import org.eclipse.lsp4j.InitializeResult
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.PublishDiagnosticsParams
import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j.ServerCapabilities
import org.eclipse.lsp4j.ServerInfo
import org.eclipse.lsp4j.TextDocumentSyncKind
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.services.LanguageClientAware
import org.eclipse.lsp4j.services.LanguageServer
import org.eclipse.lsp4j.services.TextDocumentService
import org.eclipse.lsp4j.services.WorkspaceService
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

/**
 * The language server of arg2p: the theories an editor opens are checked as they change (their syntax errors, as
 * diagnostics), and the requests of arg2p (see [Arg2pService]) solve them. Solving runs on threads of its own, with
 * the deep stack resolution needs; [exit] is what the server does when the editor tells it to exit.
 */
class Arg2pLanguageServer(
    private val exit: () -> Unit = {},
) : LanguageServer,
    LanguageClientAware,
    Arg2pService {
    private var client: Arg2pClient? = null
    private val engine = Engine { id, text -> client?.output(OutputParams(id, text)) }
    private val solving: ExecutorService =
        Executors.newCachedThreadPool { task -> Thread(null, task, "arg2p-solver", STACK_SIZE).apply { isDaemon = true } }
    private val documents = Documents()

    override fun connect(client: LanguageClient) {
        this.client = client as? Arg2pClient
    }

    override fun initialize(params: InitializeParams): CompletableFuture<InitializeResult> =
        CompletableFuture.completedFuture(
            InitializeResult(
                ServerCapabilities().apply { setTextDocumentSync(TextDocumentSyncKind.Full) },
                ServerInfo("arg2p", Arg2p.version()),
            ),
        )

    override fun shutdown(): CompletableFuture<Any> {
        solving.shutdownNow()
        return CompletableFuture.completedFuture(null)
    }

    override fun exit() = exit.invoke()

    override fun getTextDocumentService(): TextDocumentService = documents

    override fun getWorkspaceService(): WorkspaceService =
        object : WorkspaceService {
            override fun didChangeConfiguration(params: DidChangeConfigurationParams) = Unit

            override fun didChangeWatchedFiles(params: DidChangeWatchedFilesParams) = Unit
        }

    override fun solve(params: SolveParams): CompletableFuture<SolveAnswer> =
        CompletableFuture.supplyAsync({ engine.solve(params.theory ?: "", params.query ?: "", params.flags ?: "") }, solving)

    override fun next(params: RunParams): CompletableFuture<NextAnswer> = CompletableFuture.supplyAsync({ engine.next(params.id) }, solving)

    override fun graphAt(params: GraphAtParams): CompletableFuture<GraphData> =
        CompletableFuture.supplyAsync({ engine.graphAt(params.id, params.branch) }, solving)

    override fun check(params: CheckParams): CompletableFuture<ProblemData?> =
        CompletableFuture.supplyAsync({ Engine.check(params.text ?: "", params.kind ?: "theory") }, solving)

    override fun settings(params: SettingsParams): CompletableFuture<Map<String, List<String>>> =
        CompletableFuture.supplyAsync({ Engine.settings(params.theory ?: "", params.flags ?: "") }, solving)

    override fun close(params: RunParams) = engine.close(params.id)

    // The theories open in the editor, checked as they change: the syntax errors are their diagnostics
    private inner class Documents : TextDocumentService {
        override fun didOpen(params: DidOpenTextDocumentParams) = publish(params.textDocument.uri, params.textDocument.text)

        override fun didChange(params: DidChangeTextDocumentParams) {
            // the whole text, as the server asks (TextDocumentSyncKind.Full)
            params.contentChanges.lastOrNull()?.let { publish(params.textDocument.uri, it.text) }
        }

        override fun didClose(params: DidCloseTextDocumentParams) {
            client?.publishDiagnostics(PublishDiagnosticsParams(params.textDocument.uri, emptyList()))
        }

        override fun didSave(params: DidSaveTextDocumentParams) = Unit

        private fun publish(
            uri: String,
            text: String,
        ) {
            client?.publishDiagnostics(PublishDiagnosticsParams(uri, diagnostics(text)))
        }
    }

    companion object {
        /** The stack of the threads solving: resolution goes deep (the tests of arg2p run with the same) */
        const val STACK_SIZE = 512L * 1024 * 1024

        /** The diagnostics of [text], a theory: where it does not parse */
        fun diagnostics(text: String): List<Diagnostic> =
            listOfNotNull(
                Engine.check(text, "theory")?.let { problem ->
                    val line = problem.line - 1
                    val column = problem.column - 1
                    Diagnostic(
                        Range(Position(line, column), Position(line, column + problem.length)),
                        problem.message,
                        DiagnosticSeverity.Error,
                        "arg2p",
                    )
                },
            )
    }
}
