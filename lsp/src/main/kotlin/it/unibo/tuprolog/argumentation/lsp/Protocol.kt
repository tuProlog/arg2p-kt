package it.unibo.tuprolog.argumentation.lsp

import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest
import org.eclipse.lsp4j.jsonrpc.services.JsonSegment
import org.eclipse.lsp4j.services.LanguageClient
import java.util.concurrent.CompletableFuture

// The requests of arg2p beyond those of the protocol of language servers, under arg2p/: those of the engine the
// components of @tuprolog/arg2p-ui run on (see engine.ts there), so that an editor runs them on this solver, as the
// browser runs them on its own. Plain data, as JSON carries it: the parameters are read with defaults, as a client may
// leave fields out.

/** The requests an editor makes to the server */
@JsonSegment("arg2p")
interface Arg2pService {
    /** Starts solving [SolveParams.query] on [SolveParams.theory]: its run, or the error that kept it from starting */
    @JsonRequest
    fun solve(params: SolveParams): CompletableFuture<SolveAnswer>

    /** The next solution of a run, and whether there is another one */
    @JsonRequest
    fun next(params: RunParams): CompletableFuture<NextAnswer>

    /** The graph of a stage of a run */
    @JsonRequest
    fun graphAt(params: GraphAtParams): CompletableFuture<GraphData>

    /** Where a theory (or flags) or a query does not parse; null if it does */
    @JsonRequest
    fun check(params: CheckParams): CompletableFuture<ProblemData?>

    /** The values of the flags in effect for a theory solved with some flags */
    @JsonRequest
    fun settings(params: SettingsParams): CompletableFuture<Map<String, List<String>>>

    /** Forgets a run */
    @JsonNotification
    fun close(params: RunParams)
}

/** What the server tells an editor beyond the protocol of language servers */
interface Arg2pClient : LanguageClient {
    /** What a run prints */
    @JsonNotification("arg2p/output")
    fun output(params: OutputParams)
}

class SolveParams(
    var theory: String? = null,
    var query: String? = null,
    var flags: String? = null,
)

class RunParams(
    var id: Int = 0,
)

class GraphAtParams(
    var id: Int = 0,
    var branch: Int = 0,
)

class CheckParams(
    var text: String? = null,
    var kind: String? = null,
)

class SettingsParams(
    var theory: String? = null,
    var flags: String? = null,
)

class OutputParams(
    val id: Int,
    val text: String,
)

/** A run started (its id and its query, as read), or the error that kept it from starting */
data class SolveAnswer(
    val id: Int,
    val query: String? = null,
    val error: ErrorData? = null,
)

/** A solution, whether there is another one, or the error that kept it from being computed */
data class NextAnswer(
    val solution: SolutionData? = null,
    val hasNext: Boolean = false,
    val error: ErrorData? = null,
)

/** An evaluation that failed, and where its text does not parse ([ProblemData.where]: theory, flags or query) */
data class ErrorData(
    val name: String,
    val message: String,
    val syntax: ProblemData? = null,
)

data class ProblemData(
    val line: Int,
    val column: Int,
    val length: Int,
    val message: String,
    val where: String? = null,
)

data class SolutionData(
    val result: String,
    val query: String,
    val bindings: List<BindingData>,
    val error: String?,
    val graph: GraphData,
    val branch: Int,
)

data class BindingData(
    val name: String,
    val value: String,
)

data class GraphData(
    val profile: String,
    val arguments: List<ArgumentData>,
    val attacks: List<AttackData>,
) {
    companion object {
        val EMPTY = GraphData("", emptyList(), emptyList())
    }
}

data class ArgumentData(
    val id: String,
    val descriptor: String,
    val conclusion: String,
    val topRule: String,
    val label: String,
    val supports: List<String>,
)

data class AttackData(
    val from: String,
    val to: String,
    val type: String?,
    val on: String?,
)
