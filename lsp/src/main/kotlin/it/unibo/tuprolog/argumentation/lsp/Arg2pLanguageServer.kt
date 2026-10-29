package it.unibo.tuprolog.argumentation.lsp

import com.google.gson.JsonObject
import it.unibo.tuprolog.argumentation.Arg2p
import it.unibo.tuprolog.argumentation.parallel.federation.Beacons
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
 * [PageEngine]), in parallel on its federation if they ask (see [Federation], with its requests arg2p/federation/...).
 * The server reads its messages on a thread of its own: checking and solving run on threads of theirs, with the deep
 * stack resolution needs (see [Solving]); [exit] is what the server does when the editor tells it to exit.
 *
 * The editor enables the federation with the options of initialize, `{ federation: { enabled, name } }`, and is told
 * how it changes (arg2p/federationChanged); it is disabled otherwise. [beaconPort] is where its beacons go.
 */
internal class Arg2pLanguageServer(
    beaconPort: Int = Beacons.PORT,
    private val exit: () -> Unit = {},
) : LanguageServer,
    LanguageClientAware,
    Arg2pService {
    private var client: LanguageClient? = null
    private val solving = Solving("arg2p-solver")
    private val federation = Federation(solving, beaconPort) { state -> (client as? Arg2pClient)?.federationChanged(state) }
    private val pages = PageEngine(solving, parallel = federation)
    private val documents = Documents()

    override fun connect(client: LanguageClient) {
        this.client = client
    }

    override fun initialize(params: InitializeParams): CompletableFuture<InitializeResult> {
        // the options of the federation, those that are not what they should be left out
        val federated = runCatching { (params.initializationOptions as? JsonObject)?.obj("federation") }.getOrNull()
        runCatching { federated?.text("name") }.getOrNull()?.let(federation::rename)
        if (runCatching { federated?.flag("enabled") }.getOrNull() == true) {
            // started as the server answers: a failure (no network) is told, the server goes on without it
            solving.submit { federation.enable(true) }.exceptionally { failure ->
                System.err.println("The federation of arg2p did not start: ${failure.message}")
            }
        }
        return CompletableFuture.completedFuture(
            InitializeResult(
                ServerCapabilities().apply { setTextDocumentSync(TextDocumentSyncKind.Full) },
                ServerInfo("arg2p", Arg2p.version()),
            ),
        )
    }

    override fun shutdown(): CompletableFuture<Any> {
        pages.close()
        federation.close()
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
            throw invalid(failure)
        }

    // What [read] reads of [params], an error invalid params if they are not what it requires
    private fun <T> reading(
        params: JsonObject?,
        read: (JsonObject) -> T,
    ): T =
        try {
            read(params ?: throw InvalidParams("params are missing"))
        } catch (failure: InvalidParams) {
            throw invalid(failure)
        }

    override fun federation(): CompletableFuture<FederationState> = CompletableFuture.completedFuture(federation.state)

    override fun pair(params: JsonObject?): CompletableFuture<FederationState> {
        val (site, code) = reading(params) { it.requiredText("site") to it.requiredText("code") }
        // waited for on a thread of its own: the server goes on reading meanwhile
        return federating { federation.pair(site, code) }
    }

    override fun newCode(): CompletableFuture<FederationState> = federating { federation.newCode() }

    override fun rename(params: JsonObject?): CompletableFuture<FederationState> {
        val name = reading(params) { it.requiredText("name") }
        return federating { federation.rename(name) }
    }

    override fun enable(params: JsonObject?): CompletableFuture<FederationState> {
        val enabled = reading(params) { it.flag("enabled") ?: throw InvalidParams("enabled is missing") }
        return federating { federation.enable(enabled) }
    }

    // [change] of the federation done on a thread of its own, answered with its state: its failure (a code refused, the
    // federation unable to listen) is the error of the request
    private fun federating(change: () -> Unit): CompletableFuture<FederationState> =
        solving.submit(change).handling { _, failure ->
            failure?.unwrapped()?.let { cause ->
                throw ResponseErrorException(ResponseError(ResponseErrorCode.RequestFailed, cause.message ?: cause.toString(), null))
            }
            federation.state
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
        // [failure] as the error invalid params of JSON-RPC
        private fun invalid(failure: InvalidParams) =
            ResponseErrorException(ResponseError(ResponseErrorCode.InvalidParams, failure.message, null))

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
