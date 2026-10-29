package it.unibo.tuprolog.argumentation.actor.actors.splitting

import it.unibo.tuprolog.argumentation.actor.actors.HelpWorker
import it.unibo.tuprolog.argumentation.actor.message.KbMessage
import it.unibo.tuprolog.argumentation.actor.message.Stop

// The knowledge base split among the workers, each with its rules: a worker removed is stopped, since its evaluator,
// never passivated, would keep its solver otherwise
internal class SplitKnowledgeBase {
    private val split: MutableList<HelpWorker> = mutableListOf()

    /** The workers of the knowledge base */
    val workers: List<HelpWorker> get() = split

    fun add(worker: HelpWorker) {
        split += worker
    }

    /** The [retired] workers removed and stopped */
    fun remove(retired: List<HelpWorker>) {
        retired.forEach { it.ref.tell(Stop) }
        split.removeAll(retired)
    }

    fun reset() = remove(split.toList())

    fun broadcast(msg: KbMessage) = split.forEach { it.ref.tell(msg) }

    fun splitsNumber() = split.count()
}
