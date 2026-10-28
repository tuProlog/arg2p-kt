@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.parallel.message

import it.unibo.tuprolog.argumentation.Answer
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.core.Term
import kotlin.js.JsExport
import kotlin.js.JsName
import it.unibo.tuprolog.solve.Solution as LogicSolution

/**
 * An argument for the statement evaluated ([claim], the statement), with the argument itself in [queryChain] (none
 * for a statement no argument concludes), as Prolog text
 */
@JsExport
data class Response(
    val claim: String,
    @JsExport.Ignore @JsName("queryChainList") val queryChain: List<String>,
) {
    /** The argument, as Prolog text (an array in JavaScript) */
    @JsName("queryChain")
    val queryChainArray: Array<String> get() = queryChain.toTypedArray()
}

/**
 * An argument, by its global identifier ([id], the worker that built it and its identifier there), with the identifiers
 * of the arguments it rests on directly ([supports], from a worker to the catalog only)
 */
internal data class SharedArgument(
    val id: String,
    val argument: Term,
    val supports: List<String> = emptyList(),
)

/** The premises of the rules of [worker], and their conclusions */
internal data class Interest(
    val worker: Int,
    val premises: List<Term>,
    val conclusions: List<Term>,
)

/** A direct attack of type [type] of the argument [attacker] on the argument [target], by their global identifiers */
internal data class DirectAttack(
    val type: String,
    val attacker: String,
    val target: String,
)

/**
 * The messages between the nodes of a distributed solver: the catalog, the workers and the clients, each known by its
 * address (see [Address]). On a machine the terms travel as they are (they are immutable, shared by the nodes
 * without copies); the transports between processes write them as Prolog text (see [MessageCodec]).
 *
 * After a change of the theory, the first evaluation builds the graph:
 * 1. the catalog sends each worker its rules and the flags ([Load]); each tells the premises and the conclusions of
 *    its rules ([Subscribe]);
 * 2. the catalog sends every worker those of all ([Build]). On the whole graph, each builds the arguments of its rules,
 *    sends them to the workers whose rules use their conclusions ([Notify]), which build on them in turn, and
 *    publishes them to the catalog ([Publish]). In query mode, the arguments the evaluations depend on alone, as the
 *    standard solver explores them (see the module queryExploration): the catalog asks the workers concluding the
 *    statements evaluated to explore them ([Explore]); each explores them on its rules, asks the others for the
 *    statements its exploration needs and its rules do not conclude, and sends what it builds to those that asked for
 *    it. Every Build, Explore, Notify and Publish is acknowledged to its sender ([Ack]); a worker acknowledges the
 *    message that engaged it only once all it sent is acknowledged (Dijkstra and Scholten): with every Build and
 *    Explore acknowledged, the arguments are all built and published;
 * 3. the workers find the attacks ([Attack]): each tells the others the statements that may attack its arguments
 *    ([Wanted]), sends each the arguments of its own for them ([Candidates]), finds the direct attacks on its own and
 *    sends them to the catalog ([AttacksFound]), which extends them to the arguments built on the attacked ones and
 *    labels the graph.
 *
 * In query mode a later evaluation needing more of the graph explores it further ([Explore]), then the attacks are
 * found again.
 *
 * The goals of the clients ([RequireSolution], [RequireAnswer], [RequireEvaluation]) are solved by the catalog, on the
 * graph the workers built for them as above (see Catalog).
 */
sealed interface KbMessage

/** To the catalog: forget the theory */
internal data object Reset : KbMessage

/** To the catalog: a graph being built lost some of its workers (a site left), build it again with those left */
internal data object Rebuild : KbMessage

/**
 * To the catalog: the whole theory, as Prolog text, partitioned among the workers (see TheoryPartitioner); to a
 * worker: its part of the theory, replacing the one it had, and the [flags] of the solver, as Prolog facts
 */
internal data class Load(
    val theory: String,
    val flags: String = "",
) : KbMessage

/** To the catalog: a clause more for the theory, as Prolog text */
internal data class Add(
    val elem: String,
) : KbMessage

/** To the catalog: the labels of the arguments for the statement [elem], to send to the client [replyTo] */
internal data class RequireEvaluation(
    val elem: String,
    val replyTo: String,
) : KbMessage

/** To a client: the arguments for the statement it asked about, by label, or the [error] that kept it from an answer */
@JsExport
data class EvaluationResponse(
    @JsExport.Ignore @JsName("inArgsList") val inArgs: List<Response>,
    @JsExport.Ignore @JsName("outArgsList") val outArgs: List<Response>,
    @JsExport.Ignore @JsName("undArgsList") val undArgs: List<Response>,
    val error: String? = null,
) : KbMessage {
    /** The arguments labelled in (an array in JavaScript) */
    @JsName("inArgs")
    val inArray: Array<Response> get() = inArgs.toTypedArray()

    /** The arguments labelled out (an array in JavaScript) */
    @JsName("outArgs")
    val outArray: Array<Response> get() = outArgs.toTypedArray()

    /** The arguments labelled und (an array in JavaScript) */
    @JsName("undArgs")
    val undArray: Array<Response> get() = undArgs.toTypedArray()
}

/**
 * To the catalog: the next solution of the goal the client solves in [run], to send to the client [replyTo]; [goal], as
 * Prolog text, for the first one
 */
internal data class RequireSolution(
    val run: String,
    val goal: String?,
    val replyTo: String,
) : KbMessage

/** To the catalog: the statements for [query], as Prolog text, by label (see answerQuery/4), to send to the client [replyTo] */
internal data class RequireAnswer(
    val query: String,
    val replyTo: String,
) : KbMessage

/**
 * To a client: a solution of the goal of [run] (none if it has no more), the graph of its resolution, whether the goal
 * has more solutions, and what the solver printed computing it; or the [error] that kept it from one (a goal that
 * cannot be read, a theory that cannot be read). Only for the clients of the site of the catalog: never written.
 */
internal data class SolutionResponse(
    val run: String,
    val solution: LogicSolution?,
    val graph: Graph,
    val hasNext: Boolean,
    val output: String,
    val error: String? = null,
) : KbMessage

/** To a client: the [answer] of a query, or the [error] that kept it from one. Only for the clients of the site of the catalog: never written. */
internal data class AnswerResponse(
    val answer: Answer?,
    val error: String? = null,
) : KbMessage

/** To the catalog: the statements the rules of [worker] use as premises, whose arguments it needs, and conclude */
internal data class Subscribe(
    val worker: Int,
    val premises: List<Term>,
    val conclusions: List<Term>,
) : KbMessage

/**
 * To a worker: the graph is to be built, [interests] the premises and conclusions of every worker. With no [locality],
 * the arguments of all its rules; otherwise (query mode, directional or connected) those [Explore] asks for alone.
 */
internal data class Build(
    val interests: List<Interest>,
    val locality: String? = null,
) : KbMessage

/**
 * To a worker, from the catalog ([from] -1) or another worker: explore [statements] on its rules, as the standard solver
 * explores a query (see the module queryExploration), and send the arguments for them to the sender
 */
internal data class Explore(
    val from: Int,
    val statements: List<Term>,
) : KbMessage

/** To a worker: arguments of the worker [from] for the premises of its rules, to build on */
internal data class Notify(
    val from: Int,
    val arguments: List<SharedArgument>,
) : KbMessage

/** To the catalog: the arguments [worker] built, with their supports */
internal data class Publish(
    val worker: Int,
    val arguments: List<SharedArgument>,
) : KbMessage

/** To the sender of a Build, an Explore, a Notify or a Publish: it is handled, and so is all it led to; [from] is -1 for the catalog */
internal data class Ack(
    val from: Int,
) : KbMessage

/** To a worker: the arguments are all built, find the attacks on its own */
internal data object Attack : KbMessage

/** To another worker: the statements whose arguments may attack those of [worker] */
internal data class Wanted(
    val worker: Int,
    val statements: List<Term>,
) : KbMessage

/**
 * To another worker: the arguments of [worker] for the statements it wants, as much of them as an attack needs (their
 * top rules, conclusions, bodies and info, not their rules)
 */
internal data class Candidates(
    val worker: Int,
    val arguments: List<SharedArgument>,
) : KbMessage

/** To the catalog: the direct attacks on the arguments of [worker] */
internal data class AttacksFound(
    val worker: Int,
    val attacks: List<DirectAttack>,
) : KbMessage

/** The addresses of the nodes of a distributed solver */
internal object Address {
    /** The catalog, holding the theory, partitioning it and gathering the graph */
    const val CATALOG = "catalog"

    private const val WORKER = "worker_"

    /** The worker of the partition [index] */
    fun worker(index: Int) = "$WORKER$index"

    /** Whether [address] is the one of a worker */
    fun isWorker(address: String) = address.startsWith(WORKER)

    /** The index of the partition of the worker at [address] */
    fun indexOf(address: String) = address.removePrefix(WORKER).toInt()
}
