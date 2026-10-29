package it.unibo.tuprolog.argumentation.actor.actors

import akka.actor.typed.ActorRef
import akka.cluster.sharding.typed.javadsl.EntityRef
import it.unibo.tuprolog.argumentation.actor.message.AttackerResponse
import it.unibo.tuprolog.argumentation.actor.message.EvalResponse
import it.unibo.tuprolog.argumentation.actor.message.KbMessage
import it.unibo.tuprolog.solve.MutableSolver

internal data class HelpWorker(
    val id: String,
    val ref: EntityRef<KbMessage>,
    val theory: MutableSolver,
    val rules: MutableList<String>,
)

internal data class EvaluationCache(
    val id: String,
    val query: String,
    val expected: ExpectedResponseCount,
    val caller: ActorRef<KbMessage>,
    val responses: MutableList<EvalResponse> = mutableListOf(),
)

internal data class ActiveQuery(
    val type: Int,
    val id: String,
    val query: String,
    val argument: String,
    val replyTo: ActorRef<KbMessage>,
    val parentId: String = "",
    val expected: ExpectedResponseCount = ExpectedResponseCount(),
    var completed: Boolean = false,
    val responses: MutableList<AttackerResponse> = mutableListOf(),
)

// The responses to a request broadcast to the workers: each worker tells how many it sends (one at least), then sends them
internal class ExpectedResponseCount(
    var workers: Int = -1,
) {
    private var reported = 0
    private var responses = 0

    fun report(number: Int) {
        reported++
        responses += number
    }

    fun reached(received: Int) = reported == workers && received == responses
}
