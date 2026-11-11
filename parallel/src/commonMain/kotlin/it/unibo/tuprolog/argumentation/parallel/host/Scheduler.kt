package it.unibo.tuprolog.argumentation.parallel.host

import it.unibo.tuprolog.argumentation.ExperimentalArg2pApi
import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi

/** Tasks run one at a time, in the order they were posted */
@ExperimentalArg2pApi
fun interface TaskQueue {
    fun post(task: () -> Unit)

    /**
     * The task running now interrupted, where it can be (a thread of the JVM: a resolution halts at its next step),
     * none otherwise
     */
    fun interrupt() = Unit
}

/** Where the parts of a node run: each has a queue of its own, and the queues may run in parallel */
@ExperimentalArg2pApi
fun interface Scheduler {
    fun queue(): TaskQueue

    /**
     * How to run a long task on a thread of its own, beside the queues, if the scheduler has threads to: the solvers
     * building a graph together, each waiting for the others (see SharedGraph). Null where it has not.
     */
    fun apart(): ((() -> Unit) -> Unit)? = null
}

/**
 * Every task on the thread that posts it, in the order they were posted, those of all queues together: a task
 * posted while another runs waits for it, so that the nodes run in turn, in a deterministic order. A task that
 * fails leaves those after it queued, for the next post.
 */
@ExperimentalArg2pApi
class SequentialScheduler :
    Scheduler,
    TaskQueue {
    private val tasks = ArrayDeque<() -> Unit>()
    private var running = false

    override fun queue(): TaskQueue = this

    override fun post(task: () -> Unit) {
        tasks.addLast(task)
        if (running) return
        running = true
        try {
            while (tasks.isNotEmpty()) tasks.removeFirst()()
        } finally {
            running = false
        }
    }
}

/** The result of [block], with no other thread running a block locked on [lock] meanwhile (on the platforms with threads) */
@InternalArg2pApi
expect fun <T> locked(
    lock: Any,
    block: () -> T,
): T
