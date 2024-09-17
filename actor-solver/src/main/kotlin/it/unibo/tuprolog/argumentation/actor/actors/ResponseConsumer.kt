package it.unibo.tuprolog.argumentation.actor.actors

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.Props
import akka.actor.typed.javadsl.AbstractBehavior
import akka.actor.typed.javadsl.ActorContext
import akka.actor.typed.javadsl.Behaviors
import akka.actor.typed.javadsl.Receive
import it.unibo.tuprolog.argumentation.actor.message.EvaluationResponse
import it.unibo.tuprolog.argumentation.actor.message.KbMessage
import it.unibo.tuprolog.argumentation.actor.message.RequireEvaluation
import java.util.concurrent.CompletableFuture
import kotlin.random.Random

class ResponseConsumer private constructor(
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
            }
            .build()

    companion object {
        private fun create(consumer: CompletableFuture<EvaluationResponse>): Behavior<KbMessage> =
            Behaviors.setup { context -> ResponseConsumer(context, consumer) }

        fun getResponse(
            goal: String,
            actorSystem: ActorSystem<KbMessage>,
            master: ActorRef<KbMessage>,
        ): EvaluationResponse {
            val future = CompletableFuture<EvaluationResponse>()
            val actor = actorSystem.systemActorOf(create(future), "listener_${Random.nextInt()}", Props.empty())
            master.tell(RequireEvaluation(goal, actor))
            return future.join()
        }
    }
}
