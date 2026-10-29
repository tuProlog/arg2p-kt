package it.unibo.tuprolog.argumentation.parallel.federation

/** Tasks run one at a time, in the order they were posted */
fun interface TaskQueue {
    fun post(task: () -> Unit)

    /**
     * The task running now interrupted, where it can be (a thread of the JVM: a resolution halts at its next step),
     * none otherwise
     */
    fun interrupt() = Unit
}

/** Where the nodes of a site run: each node has a queue of its own, and the queues may run in parallel */
fun interface Scheduler {
    fun queue(): TaskQueue
}

/**
 * Every task on the thread that posts it, in the order they were posted, those of all queues together: a task
 * posted while another runs waits for it, so that the nodes run in turn, in a deterministic order. A task that
 * fails leaves those after it queued, for the next post.
 */
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

// The result of [block], with no other thread running a block locked on [lock] meanwhile (on the platforms with threads)
internal expect fun <T> locked(
    lock: Any,
    block: () -> T,
): T
