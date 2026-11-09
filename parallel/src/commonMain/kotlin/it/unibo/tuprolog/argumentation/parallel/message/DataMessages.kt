@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.parallel.message

import it.unibo.tuprolog.argumentation.Answer
import it.unibo.tuprolog.argumentation.core.model.CompactGraph
import it.unibo.tuprolog.argumentation.core.model.emptyCompactGraph
import kotlin.js.JsExport
import kotlin.js.JsName
import it.unibo.tuprolog.solve.Solution as LogicSolution

/**
 * A statement answering the one evaluated ([claim]: the statement itself, or, with constraints, one of its instances),
 * with the arguments for it in [queryChain] (none for a statement no argument concludes), as Prolog text
 */
@JsExport
data class Response(
    val claim: String,
    @JsExport.Ignore @JsName("queryChainList") val queryChain: List<String>,
) {
    /** The arguments for the statement, as Prolog text (an array in JavaScript) */
    @JsName("queryChain")
    val queryChainArray: Array<String> get() = queryChain.toTypedArray()
}

/**
 * What the clients of a node ask its solver (see ClientSolver), and what they are answered: the theory, the flags, the
 * goals ([RequireSolution], [RequireAnswer], [RequireEvaluation]) solved as the standard solver solves them
 */
sealed interface KbMessage

/** To the solver: forget the theory */
internal data object Reset : KbMessage

/** To the solver: a host running consumers of its graphs left: they are built anew */
internal data object Rebuild : KbMessage

/**
 * To the solver: the evaluations of the clients were stopped (they were told already), give up the graph being built
 * for them; nothing to do if none was
 */
internal data object Stop : KbMessage

/** To the solver: the flags of the solver, as Prolog facts, for the evaluations from now on (the graph built anew) */
internal data class Configure(
    val flags: String,
) : KbMessage

/** To the solver: the whole theory, as Prolog text, replacing the one it had */
internal data class Load(
    val theory: String,
) : KbMessage

/** To the solver: a clause more for the theory, as Prolog text */
internal data class Add(
    val elem: String,
) : KbMessage

/** To the solver: the labels of the statements answering the statement [elem], to send to the client [replyTo] */
internal data class RequireEvaluation(
    val elem: String,
    val replyTo: String,
) : KbMessage

/**
 * To a client: the statements answering the statement it asked about, by label, as solve/4 labels them, each with
 * the arguments for it, or the [error] that kept it from an answer
 */
@JsExport
data class EvaluationResponse(
    @JsExport.Ignore @JsName("inArgsList") val inArgs: List<Response>,
    @JsExport.Ignore @JsName("outArgsList") val outArgs: List<Response>,
    @JsExport.Ignore @JsName("undArgsList") val undArgs: List<Response>,
    val error: String? = null,
) : KbMessage {
    /** The statements labelled in (an array in JavaScript) */
    @JsName("inArgs")
    val inArray: Array<Response> get() = inArgs.toTypedArray()

    /** The statements labelled out (an array in JavaScript) */
    @JsName("outArgs")
    val outArray: Array<Response> get() = outArgs.toTypedArray()

    /** The statements labelled und (an array in JavaScript) */
    @JsName("undArgs")
    val undArray: Array<Response> get() = undArgs.toTypedArray()
}

/**
 * To the solver: the next solution of the goal the client solves in [run], to send to the client [replyTo]; [goal], as
 * Prolog text, for the first one
 */
internal data class RequireSolution(
    val run: String,
    val goal: String?,
    val replyTo: String,
) : KbMessage

/** To the solver: the statements for [query], as Prolog text, by label (see solve/4), to send to the client [replyTo] */
internal data class RequireAnswer(
    val query: String,
    val replyTo: String,
) : KbMessage

/**
 * To a client: a solution of the goal of [run] (none if it has no more), the graph of its resolution, and what the
 * solver printed computing it; or the [error] that kept it from one (a goal that cannot be read, a theory that cannot
 * be read); or, [stopped], none as the evaluation was stopped. Only for the clients of the node: never
 * written.
 */
internal data class SolutionResponse(
    val run: String,
    val solution: LogicSolution?,
    val graph: CompactGraph,
    val output: String,
    val error: String? = null,
    val stopped: Boolean = false,
) : KbMessage {
    /** Whether the goal has no more solutions: no solution here, no error, not stopped */
    val exhausted: Boolean get() = solution == null && error == null && !stopped
}

/** The graph of a solution that labels none */
internal val NO_GRAPH = emptyCompactGraph()

/** Why an evaluation stopped by its client has no answer */
internal const val STOPPED = "The evaluation was stopped"

/** To a client: the [answers] of a query, or the [error] that kept it from one. Only for the clients of the node: never written. */
internal data class AnswerResponse(
    val answers: List<Answer>?,
    val error: String? = null,
) : KbMessage
