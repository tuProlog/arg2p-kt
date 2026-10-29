package it.unibo.tuprolog.argumentation.parallel.federation

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.EquivalenceScenarios
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import kotlin.random.Random

/**
 * The tasks of every queue in the order they were posted, the queues taking turns at random (with [seed]): the nodes
 * interleaved as threads may interleave them, the messages from a node to another in order, deterministically
 */
class InterleavedScheduler(
    seed: Int,
) : Scheduler {
    private val random = Random(seed)
    private val queues = mutableListOf<ArrayDeque<() -> Unit>>()
    private var running = false

    override fun queue(): TaskQueue {
        val tasks = ArrayDeque<() -> Unit>().also { queues += it }
        return TaskQueue { task ->
            tasks.addLast(task)
            run()
        }
    }

    private fun run() {
        if (running) return
        running = true
        try {
            while (true) {
                val ready = queues.filter { it.isNotEmpty() }
                if (ready.isEmpty()) break
                ready.random(random).removeFirst()()
            }
        } finally {
            running = false
        }
    }
}

// The scenarios on three federated sites whose nodes are interleaved at random, as their threads may be: the two
// lending their workers linked only to the one evaluating, which relays their messages
abstract class InterleavedLocalClusterTest(
    private val seed: Int,
) : EquivalenceScenarios() {
    override fun distributed(
        theory: String,
        flags: Flags,
    ): (String) -> EvaluationResponse {
        val scheduler = InterleavedScheduler(seed)
        val evaluating = LocalSite(1, scheduler, flags)
        val first = LocalSite(2, scheduler)
        val second = LocalSite(2, scheduler)
        MemoryLinks.connect(evaluating.site, first.site, scheduler.queue())
        MemoryLinks.connect(evaluating.site, second.site, scheduler.queue())
        evaluating.load(theory)
        return evaluating::solve
    }
}

class InterleavedLocalClusterTest1 : InterleavedLocalClusterTest(1)

class InterleavedLocalClusterTest2 : InterleavedLocalClusterTest(2)

class InterleavedLocalClusterTest3 : InterleavedLocalClusterTest(3)
