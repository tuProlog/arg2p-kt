package it.unibo.tuprolog.argumentation.actor.actors

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.Props
import akka.actor.typed.javadsl.AbstractBehavior
import akka.actor.typed.javadsl.ActorContext
import akka.actor.typed.javadsl.Behaviors
import akka.actor.typed.javadsl.Receive
import it.unibo.tuprolog.argumentation.actor.ClusterException
import it.unibo.tuprolog.argumentation.actor.message.EvaluationAborted
import it.unibo.tuprolog.argumentation.actor.message.EvaluationResponse
import it.unibo.tuprolog.argumentation.actor.message.KbMessage
import it.unibo.tuprolog.argumentation.actor.message.RequireEvaluation
import java.util.UUID
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutionException
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeoutException
import kotlin.time.Duration
import kotlin.time.Duration.Companion.seconds

// The listener of the answer to an evaluation, which it completes [consumer] with, stopping then
internal class ResponseConsumer private constructor(
    context: ActorContext<KbMessage>,
    private val consumer: CompletableFuture<EvaluationResponse>,
) : AbstractBehavior<KbMessage>(
        context,
    ) {
    override fun createReceive(): Receive<KbMessage> =
        newReceiveBuilder()
            .onMessage(EvaluationResponse::class.java) { command ->
                consumer.complete(command)
                Behaviors.stopped()
            }.onMessage(EvaluationAborted::class.java) { command ->
                consumer.completeExceptionally(ClusterException(command.reason))
                Behaviors.stopped()
            }.build()

    companion object {
        /** How long an evaluation is waited for, by default */
        val DEFAULT_TIMEOUT: Duration = 60.seconds

        private fun create(consumer: CompletableFuture<EvaluationResponse>): Behavior<KbMessage> =
            Behaviors.setup { context -> ResponseConsumer(context, consumer) }

        /**
         * The labels of the arguments for [goal], evaluated by the cluster of [actorSystem] whose knowledge base [master]
         * distributes: a [ClusterException] if it has no answer within [timeout] (a message lost to a failure, say), or
         * the evaluation is given up
         */
        fun getResponse(
            goal: String,
            actorSystem: ActorSystem<KbMessage>,
            master: ActorRef<KbMessage>,
            timeout: Duration = DEFAULT_TIMEOUT,
        ): EvaluationResponse {
            val future = CompletableFuture<EvaluationResponse>()
            val listener = actorSystem.systemActorOf(create(future), "listener_${UUID.randomUUID()}", Props.empty())
            master.tell(RequireEvaluation(goal, listener))
            return try {
                future.get(timeout.inWholeMilliseconds, TimeUnit.MILLISECONDS)
            } catch (_: TimeoutException) {
                val reason = "No answer from the cluster for $goal within $timeout"
                // the listener stopped, as an answer would stop it
                listener.tell(EvaluationAborted(reason))
                throw ClusterException(reason)
            } catch (e: ExecutionException) {
                throw e.cause as? ClusterException ?: ClusterException(e.cause?.message ?: e.toString())
            }
        }
    }
}
