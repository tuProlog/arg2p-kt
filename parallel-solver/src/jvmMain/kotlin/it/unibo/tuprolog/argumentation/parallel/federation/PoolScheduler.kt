package it.unibo.tuprolog.argumentation.parallel.federation

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicBoolean

/**
 * The queues run on a pool of [threads] daemon threads, each queue on one thread at a time: a task that fails is
 * skipped, as an actor resumed on a failure does
 */
class PoolScheduler(
    threads: Int,
) : Scheduler,
    AutoCloseable {
    private val executor: ExecutorService =
        Executors.newFixedThreadPool(threads) { task -> Thread(task, "arg2p-parallel").apply { isDaemon = true } }

    override fun queue(): TaskQueue = Mailbox()

    /** Stops the threads */
    override fun close() {
        executor.shutdownNow()
    }

    private inner class Mailbox : TaskQueue {
        private val tasks = ConcurrentLinkedQueue<() -> Unit>()
        private val scheduled = AtomicBoolean(false)

        // The thread running a task of the queue now, if one is: an interruption reaches that task alone
        private val lock = Any()
        private var running: Thread? = null

        override fun interrupt() = synchronized(lock) { running?.interrupt() ?: Unit }

        override fun post(task: () -> Unit) {
            tasks.add(task)
            schedule()
        }

        private fun schedule() {
            if (scheduled.compareAndSet(false, true)) executor.execute(::drain)
        }

        private fun drain() {
            try {
                while (true) {
                    val task = tasks.poll() ?: break
                    synchronized(lock) { running = Thread.currentThread() }
                    runCatching(task).onFailure { it.printStackTrace() }
                    // an interruption of the task not left to the next ones on the thread, of any queue
                    synchronized(lock) {
                        running = null
                        Thread.interrupted()
                    }
                }
            } finally {
                scheduled.set(false)
                // a task posted after the last poll, before scheduled was reset
                if (tasks.isNotEmpty()) schedule()
            }
        }
    }
}
