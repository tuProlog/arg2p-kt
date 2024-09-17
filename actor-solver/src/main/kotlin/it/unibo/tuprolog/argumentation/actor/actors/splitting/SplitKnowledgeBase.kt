package it.unibo.tuprolog.argumentation.actor.actors.splitting

import it.unibo.tuprolog.argumentation.actor.actors.HelpWorker
import it.unibo.tuprolog.argumentation.actor.message.KbMessage

class SplitKnowledgeBase {
    val workers: MutableList<HelpWorker> = mutableListOf()

    fun reset() = workers.clear()

    fun broadcast(msg: KbMessage) = workers.forEach { it.ref.tell(msg) }

    fun splitsNumber() = workers.count()
}
