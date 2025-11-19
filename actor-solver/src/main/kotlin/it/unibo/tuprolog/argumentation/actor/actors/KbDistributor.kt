package it.unibo.tuprolog.argumentation.actor.actors

import akka.actor.typed.Behavior
import akka.actor.typed.javadsl.AbstractBehavior
import akka.actor.typed.javadsl.ActorContext
import akka.actor.typed.javadsl.Behaviors
import akka.actor.typed.javadsl.Receive
import it.unibo.tuprolog.argumentation.actor.actors.splitting.SplitKnowledgeBase
import it.unibo.tuprolog.argumentation.actor.actors.splitting.SupportSplit
import it.unibo.tuprolog.argumentation.actor.message.Add
import it.unibo.tuprolog.argumentation.actor.message.Eval
import it.unibo.tuprolog.argumentation.actor.message.EvalResponse
import it.unibo.tuprolog.argumentation.actor.message.EvaluationResponse
import it.unibo.tuprolog.argumentation.actor.message.ExpectedResponses
import it.unibo.tuprolog.argumentation.actor.message.FindAttacker
import it.unibo.tuprolog.argumentation.actor.message.KbMessage
import it.unibo.tuprolog.argumentation.actor.message.Label
import it.unibo.tuprolog.argumentation.actor.message.RequireEvaluation
import it.unibo.tuprolog.argumentation.actor.message.Reset
import it.unibo.tuprolog.argumentation.actor.message.Response
import kotlin.random.Random

class KbDistributor private constructor(
    context: ActorContext<KbMessage>,
) : AbstractBehavior<KbMessage>(context) {
    private val kb: SplitKnowledgeBase = SplitKnowledgeBase()
    private val evaluationCache: MutableList<EvaluationCache> = mutableListOf()

    override fun createReceive(): Receive<KbMessage> =
        newReceiveBuilder()
            .onMessage(Reset::class.java) {
                context.log.info("Resetting")
                kb.reset()
                evaluationCache.clear()
                this
            }.onMessage(Add::class.java) { command ->
                context.log.info("Adding ${command.elem}")
                SupportSplit.updateWorkers(command.elem, kb, context)
                this
            }.onMessage(RequireEvaluation::class.java) { command ->
                context.log.info("Eval for ${command.elem}")
                val cache = EvaluationCache("eval_${Random.nextInt(0, Int.MAX_VALUE)}", command.elem, kb.splitsNumber(), command.replyTo)
                evaluationCache.add(cache)
                kb.broadcast(Eval(cache.id, command.elem))
                this
            }.onMessage(EvalResponse::class.java) { command ->
                evaluationCache.firstOrNull { it.id == command.id }?.let {
                    it.responses.add(command)
                    evaluateResponses(it)
                }
                this
            }.onMessage(FindAttacker::class.java) { command ->
                command.replyTo.tell(ExpectedResponses(command.id, kb.splitsNumber()))
                kb.broadcast(command)
                this
            }.build()

    private fun evaluateResponses(cache: EvaluationCache) {
        if (cache.responses.size < cache.responseNumber) return

        val filter = { label: Label ->
            cache.responses.filter { it.response == label }.map {
                Response(
                    it.elem,
                    it.queryChain,
                )
            }
        }

        cache.caller.tell(
            EvaluationResponse(
                inArgs = filter(Label.IN),
                outArgs = filter(Label.OUT),
                undArgs =
                    if (cache.responses.any { it.response != Label.NOT_FOUND }) {
                        filter(Label.UND)
                    } else {
                        filter(Label.NOT_FOUND).distinct()
                    },
            ),
        )

        evaluationCache.remove(cache)
    }

    companion object {
        fun create(): Behavior<KbMessage> = Behaviors.setup { context -> KbDistributor(context) }
    }
}
