package it.unibo.tuprolog.argumentation.actor.message

import akka.actor.typed.ActorRef

enum class Label {
    IN,
    OUT,
    UND,
    NOT_FOUND,
}

data class Response(
    val claim: String,
    val queryChain: List<String>,
)

interface KbMessage

object Reset : KbMessage

data class Add(val elem: String) : KbMessage

data class RequireEvaluation(val elem: String, val replyTo: ActorRef<KbMessage>) : KbMessage

data class EvaluationResponse(val inArgs: List<Response>, val outArgs: List<Response>, val undArgs: List<Response>) : KbMessage

data class Eval(val id: String, val elem: String) : KbMessage

data class EvalResponse(val id: String, val elem: String, val response: Label, val queryChain: List<String>) : KbMessage

data class FindAttacker(val id: String, val argument: String, val queryChain: List<String>, val replyTo: ActorRef<KbMessage>) : KbMessage

data class ExpectedResponses(val id: String, val number: Int) : KbMessage

data class AttackerResponse(val id: String, val argument: String, val queryChain: List<String>, val response: Label) : KbMessage
