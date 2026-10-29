package it.unibo.tuprolog.argumentation.lsp

import com.google.gson.JsonArray
import com.google.gson.JsonObject
import com.google.gson.JsonParser
import com.google.gson.TypeAdapter
import com.google.gson.annotations.JsonAdapter
import com.google.gson.stream.JsonReader
import com.google.gson.stream.JsonWriter
import it.unibo.tuprolog.argumentation.Solution
import it.unibo.tuprolog.argumentation.SyntaxProblem
import it.unibo.tuprolog.argumentation.core.model.Graph
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest
import org.eclipse.lsp4j.jsonrpc.services.JsonSegment
import org.eclipse.lsp4j.services.LanguageClient
import java.util.concurrent.CompletableFuture

// What the servers tell of theories, as plain data: the messages of the pages of the solver to their engine (see
// PageEngine), as the components of @tuprolog/arg2p-ui read them (see engine.ts there), and the results of the tools of
// the MCP server.

/**
 * The requests of arg2p beyond those of the protocol of language servers, under arg2p/: the engine of the pages, and
 * the federation of the server (see [Federation]), whose requests answer with its state
 */
@JsonSegment("arg2p")
internal interface Arg2pService {
    /** The answers of the engine of the pages to a request of a page (see [PageEngine]) */
    @JsonRequest
    fun engine(params: EngineParams): CompletableFuture<EngineAnswers>

    /** The state of the federation */
    @JsonRequest
    fun federation(): CompletableFuture<FederationState>

    /** The site paired with the site { site, code } nearby, presenting its code: an error if it refuses it */
    @JsonRequest("federation/pair")
    fun pair(params: JsonObject?): CompletableFuture<FederationState>

    /** A new code of the site, the one before refused from now on */
    @JsonRequest("federation/newCode")
    fun newCode(): CompletableFuture<FederationState>

    /** The site named { name } (blank: the name of the host) */
    @JsonRequest("federation/rename")
    fun rename(params: JsonObject?): CompletableFuture<FederationState>

    /** The federation enabled, or disabled { enabled } */
    @JsonRequest("federation/enable")
    fun enable(params: JsonObject?): CompletableFuture<FederationState>
}

/** The editor, as the server of arg2p tells it beyond the protocol of language servers */
internal interface Arg2pClient : LanguageClient {
    /** The federation changed: its state now */
    @JsonNotification("arg2p/federationChanged")
    fun federationChanged(state: FederationState)
}

/** A request of the page of [session] to its engine */
internal class EngineParams(
    var session: String? = null,
    var request: JsonObject? = null,
)

/** The answers of the engine to a request of a page, written with their null fields, as the page reads them */
@JsonAdapter(EngineAnswers.Adapter::class)
internal class EngineAnswers(
    val answers: JsonArray,
) {
    // The answers as the JSON of the servers writes them: the protocol of language servers leaves the null fields out
    class Adapter : TypeAdapter<EngineAnswers>() {
        override fun write(
            output: JsonWriter,
            value: EngineAnswers,
        ) = json.toJson(JsonObject().apply { add("answers", value.answers) }, output)

        override fun read(input: JsonReader): EngineAnswers =
            EngineAnswers(JsonParser.parseReader(input).asJsonObject.getAsJsonArray("answers"))
    }

    companion object {
        /** [answers] as JSON */
        fun of(answers: List<Answer>) = EngineAnswers(json.toJsonTree(answers).asJsonArray)
    }
}

/** A message of the engine to a page: its type, and its fields */
internal typealias Answer = Map<String, Any?>

/** An evaluation that failed, and where its text does not parse ([ProblemData.where]: theory, flags or query) */
internal data class ErrorData(
    val name: String,
    val message: String,
    val syntax: ProblemData? = null,
)

internal data class ProblemData(
    val line: Int,
    val column: Int,
    val length: Int,
    val message: String,
    val where: String? = null,
) {
    companion object {
        /** [problem], in the text [where] is about */
        fun of(
            problem: SyntaxProblem,
            where: String? = null,
        ) = ProblemData(problem.line, problem.column, problem.length, problem.description, where)
    }
}

/** A solution as the tool solve of the MCP server gives it: with no graph, the tool giving the one of the last yes alone */
internal data class SolvedData(
    val result: String,
    val query: String,
    val bindings: List<BindingData>,
    val error: String?,
) {
    companion object {
        fun of(solution: Solution) = SolvedData(solution.result, solution.query, BindingData.of(solution), solution.error)
    }
}

/** A solution as the pages read it: the [SolvedData] of the solution, with the graph of its stage */
internal data class SolutionData(
    val result: String,
    val query: String,
    val bindings: List<BindingData>,
    val error: String?,
    val graph: GraphData,
    val branch: Int,
) {
    companion object {
        /** [solution], with the graph of its stage [branch] */
        fun of(
            solution: Solution,
            graph: GraphData,
            branch: Int,
        ) = SolvedData.of(solution).let { SolutionData(it.result, it.query, it.bindings, it.error, graph, branch) }

        /** A solution that could not be computed, for [error] */
        fun halted(error: Exception) = SolutionData("halt", "", emptyList(), error.message ?: error.toString(), GraphData.EMPTY, 0)
    }
}

internal data class BindingData(
    val name: String,
    val value: String,
) {
    companion object {
        /** The values [solution] binds to the variables of its query, in their order */
        fun of(solution: Solution) = solution.variables.map { BindingData(it, solution.text(it) ?: "") }
    }
}

internal data class GraphData(
    val profile: String,
    val arguments: List<ArgumentData>,
    val attacks: List<AttackData>,
) {
    companion object {
        val EMPTY = GraphData("", emptyList(), emptyList())

        /** A graph of the API, read in [profile] */
        fun of(
            graph: Graph,
            profile: String,
        ) = GraphData(
            profile,
            graph.labellings.map { (argument, label) ->
                ArgumentData(
                    id = argument.identifier,
                    descriptor = argument.descriptor,
                    conclusion = argument.conclusion,
                    topRule = argument.topRule,
                    label = label,
                    supports = argument.supports.map { it.identifier },
                )
            },
            graph.attacks.map { AttackData(it.attacker.identifier, it.target.identifier, it.type, it.on?.identifier) },
        )
    }
}

internal data class ArgumentData(
    val id: String,
    val descriptor: String,
    val conclusion: String,
    val topRule: String,
    val label: String,
    val supports: List<String>,
)

internal data class AttackData(
    val from: String,
    val to: String,
    val type: String?,
    val on: String?,
)
