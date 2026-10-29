package it.unibo.tuprolog.argumentation.actor.message

import akka.actor.typed.ActorRef

internal enum class Label {
    IN,
    OUT,
    UND,
    NOT_FOUND,
}

internal data class Response(
    val claim: String,
    val queryChain: List<String>,
)

internal interface KbMessage

internal object Reset : KbMessage

// The worker receiving it stops, its rules given to another one or dropped with the knowledge base
internal object Stop : KbMessage

internal data class Add(
    val elem: String,
) : KbMessage

internal data class RequireEvaluation(
    val elem: String,
    val replyTo: ActorRef<KbMessage>,
) : KbMessage

internal data class EvaluationResponse(
    val inArgs: List<Response>,
    val outArgs: List<Response>,
    val undArgs: List<Response>,
) : KbMessage

// The answer to a RequireEvaluation that has none, and why (the knowledge base reset while it was evaluated, say)
internal data class EvaluationAborted(
    val reason: String,
) : KbMessage

internal data class Eval(
    val id: String,
    val elem: String,
) : KbMessage

internal data class EvalResponse(
    val id: String,
    val elem: String,
    val response: Label,
    val queryChain: List<String>,
) : KbMessage

internal data class FindAttacker(
    val id: String,
    val argument: String,
    val subArguments: List<String>,
    val queryChain: List<String>,
    val replyTo: ActorRef<KbMessage>,
) : KbMessage

internal data class ExpectedResponses(
    val id: String,
    val number: Int,
) : KbMessage

// The number of responses a worker sends to the request id, told before them
internal data class WorkerResponses(
    val id: String,
    val number: Int,
) : KbMessage

internal data class AttackerResponse(
    val id: String,
    val argument: String,
    val queryChain: List<String>,
    val response: Label,
) : KbMessage
