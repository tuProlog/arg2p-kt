package it.unibo.tuprolog.argumentation.federation

import it.unibo.tuprolog.argumentation.parallel.ParallelNode
import it.unibo.tuprolog.argumentation.parallel.host.ConsumerThreads
import it.unibo.tuprolog.argumentation.parallel.host.Scheduler

/** A site of the tests, on a node of its own offering [workers] consumers on [threads], if given */
internal fun site(
    scheduler: Scheduler,
    workers: Int?,
    id: String = ParallelNode.newId(),
    threads: ConsumerThreads? = null,
    lends: Boolean = true,
) = Site(ParallelNode(scheduler, workers, id = id, threads = threads), lends)
