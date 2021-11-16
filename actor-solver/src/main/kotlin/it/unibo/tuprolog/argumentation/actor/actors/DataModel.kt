package it.unibo.tuprolog.argumentation.actor.actors

import akka.actor.typed.ActorRef
import it.unibo.tuprolog.argumentation.actor.message.AttackerResponse
import it.unibo.tuprolog.argumentation.actor.message.EvalResponse
import it.unibo.tuprolog.argumentation.actor.message.KbMessage
import it.unibo.tuprolog.solve.MutableSolver

data class HelpWorker(
    val id: String,
    val ref: ActorRef<KbMessage>,
    val theory: MutableSolver,
    val rules: MutableList<String>
)

data class EvaluationCache(
    val id: String,
    val query: String,
    val responseNumber: Int,
    val caller: ActorRef<KbMessage>,
    val responses: MutableList<EvalResponse> = mutableListOf()
)

data class ActiveQuery(
    val type: Int,
    val id: String,
    var expectedResponses: Int,
    val query: String,
    val argument: String,
    val replyTo: ActorRef<KbMessage>,
    val parentId: String = "",
    var completed: Boolean = false,
    val responses: MutableList<AttackerResponse> = mutableListOf()
)
