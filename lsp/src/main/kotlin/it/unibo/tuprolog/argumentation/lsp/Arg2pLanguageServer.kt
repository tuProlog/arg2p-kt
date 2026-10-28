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
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException
import org.eclipse.lsp4j.jsonrpc.messages.ResponseError
import org.eclipse.lsp4j.jsonrpc.messages.ResponseErrorCode
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.services.LanguageClientAware
import org.eclipse.lsp4j.services.LanguageServer
import org.eclipse.lsp4j.services.TextDocumentService
import org.eclipse.lsp4j.services.WorkspaceService
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit

/**
 * The language server of arg2p: the theories an editor opens are checked as they change (their syntax errors, as
 * diagnostics), and the pages of the solver the editor shows solve on its engine (the request arg2p/engine, see
 * [PageEngine]). The server reads its messages on a thread of its own: checking and solving run on threads of theirs,
 * with the deep stack resolution needs (see [Solving]); [exit] is what the server does when the editor tells it to exit.
 */
internal class Arg2pLanguageServer(
    private val exit: () -> Unit = {},
) : LanguageServer,
    LanguageClientAware,
    Arg2pService {
    private var client: LanguageClient? = null
    private val solving = Solving("arg2p-solver")
    private val pages = PageEngine(solving)
    private val documents = Documents()

    override fun connect(client: LanguageClient) {
        this.client = client
    }

    override fun initialize(params: InitializeParams): CompletableFuture<InitializeResult> =
        CompletableFuture.completedFuture(
            InitializeResult(
                ServerCapabilities().apply { setTextDocumentSync(TextDocumentSyncKind.Full) },
                ServerInfo("arg2p", Arg2p.version()),
            ),
        )

    override fun shutdown(): CompletableFuture<Any> {
        pages.close()
        solving.close()
        return CompletableFuture.completedFuture(null)
    }

    override fun exit() = exit.invoke()

    override fun getTextDocumentService(): TextDocumentService = documents

    override fun getWorkspaceService(): WorkspaceService =
        object : WorkspaceService {
            override fun didChangeConfiguration(params: DidChangeConfigurationParams) = Unit

            override fun didChangeWatchedFiles(params: DidChangeWatchedFilesParams) = Unit
        }

    override fun engine(params: EngineParams): CompletableFuture<EngineAnswers> =
        try {
            val session = params.session ?: throw InvalidParams("session is missing")
            val request = params.request ?: throw InvalidParams("request is missing")
            // cancelled by the editor, the request stops
            pages.handle(session, request).handling { answers, failure -> EngineAnswers.of(answers ?: throw failure!!) }
        } catch (failure: InvalidParams) {
            throw ResponseErrorException(ResponseError(ResponseErrorCode.InvalidParams, failure.message, null))
        }

    // The theories open in the editor, checked once their text settles: the syntax errors are their diagnostics
    private inner class Documents : TextDocumentService {
        // the last text of each document, and its check to come
        private val texts = ConcurrentHashMap<String, String>()
        private val checks = ConcurrentHashMap<String, Future<*>>()

        override fun didOpen(params: DidOpenTextDocumentParams) = check(params.textDocument.uri, params.textDocument.text)

        override fun didChange(params: DidChangeTextDocumentParams) {
            // the whole text, as the server asks (TextDocumentSyncKind.Full)
            params.contentChanges.lastOrNull()?.let { check(params.textDocument.uri, it.text) }
        }

        override fun didClose(params: DidCloseTextDocumentParams) {
            val uri = params.textDocument.uri
            texts.remove(uri)
            checks.remove(uri)?.cancel(false)
            client?.publishDiagnostics(PublishDiagnosticsParams(uri, emptyList()))
        }

        override fun didSave(params: DidSaveTextDocumentParams) = Unit

        // The diagnostics of [text], published once it settles, if it is still the text of the document
        private fun check(
            uri: String,
            text: String,
        ) {
            texts[uri] = text
            val due =
                Solving.timer.schedule(
                    {
                        solving.submit {
                            val found = diagnostics(text)
                            if (texts[uri] === text) client?.publishDiagnostics(PublishDiagnosticsParams(uri, found))
                        }
                    },
                    SETTLING,
                    TimeUnit.MILLISECONDS,
                )
            checks.put(uri, due)?.cancel(false)
        }
    }

    companion object {
        /** How long a text is left unchanged before it is checked, in milliseconds: the editor sends each keystroke */
        const val SETTLING = 200L

        /** The diagnostics of [text], a theory: where it does not parse */
        fun diagnostics(text: String): List<Diagnostic> =
            listOfNotNull(
                Solvers.check(text, "theory")?.let { problem ->
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
