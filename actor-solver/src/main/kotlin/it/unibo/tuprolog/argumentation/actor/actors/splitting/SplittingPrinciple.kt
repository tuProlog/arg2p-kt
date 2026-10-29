package it.unibo.tuprolog.argumentation.actor.actors.splitting

import akka.actor.typed.javadsl.ActorContext
import it.unibo.tuprolog.argumentation.actor.message.KbMessage

internal interface SplittingPrinciple {
    fun updateWorkers(
        rule: String,
        kb: SplitKnowledgeBase,
        context: ActorContext<KbMessage>,
    )
}
