package it.unibo.tuprolog.argumentation.lsp

import java.util.concurrent.CompletableFuture
import java.util.concurrent.CompletionException
import java.util.concurrent.Executors
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.TimeUnit
import kotlin.time.Duration

/**
 * The threads the servers solve on, with the deep stack resolution needs, while they go on reading their messages: the
 * future of each work is cancelled to stop it, which interrupts its thread, and so the resolution under way (see
 * [Deadline]).
 */
internal class Solving(
    name: String,
) : AutoCloseable {
    private val executor = Executors.newCachedThreadPool { task -> Thread(null, task, name, STACK_SIZE).apply { isDaemon = true } }

    /** The result of [work], solved on a thread of its own: cancelling it interrupts the work */
    fun <T> submit(work: () -> T): CompletableFuture<T> {
        val result = CompletableFuture<T>()
        val task =
            executor.submit {
                try {
                    result.complete(work())
                } catch (failure: Throwable) {
                    result.completeExceptionally(failure)
                }
            }
        result.whenComplete { _, _ -> if (result.isCancelled) task.cancel(true) }
        return result
    }

    /** How many works are under way */
    val busy: Int get() = (executor as ThreadPoolExecutor).activeCount

    /** Stops every work, and the threads */
    override fun close() {
        executor.shutdownNow()
    }

    companion object {
        /** The stack of the threads solving: resolution goes deep (the tests of arg2p run with the same) */
        const val STACK_SIZE = 512L * 1024 * 1024

        /** The timer of the servers: the deadlines of their works, the sessions they forget */
        val timer: ScheduledExecutorService =
            Executors.newSingleThreadScheduledExecutor { task -> Thread(task, "arg2p-timer").apply { isDaemon = true } }
    }
}

/** What [handler] makes of the result of this future, or of its failure: cancelling what it makes cancels this future */
internal fun <T, R> CompletableFuture<T>.handling(handler: (T?, Throwable?) -> R): CompletableFuture<R> =
    handle(handler).also { handled -> handled.whenComplete { _, _ -> if (handled.isCancelled) cancel(true) } }

/** The failure of a future (as [handling] is given it) as its work threw it: out of the CompletionException wrapping it */
internal fun Throwable.unwrapped(): Throwable = (this as? CompletionException)?.cause ?: this

/** A work that ran out of its time, [timeout]: what it fails with wherever it runs (see [Deadline.timed]) */
internal class TimedOut(
    timeout: Duration,
) : Exception("No answer within ${timeout.inWholeSeconds} seconds: the theory may not terminate, or need more time.")

/**
 * The time limit of a work, on the thread it runs on: past it, the thread is interrupted, which stops the resolution
 * under way (it halts); the work tells by [expired] that it ran out of time, to answer with what it found so far.
 */
internal class Deadline private constructor(
    private val thread: Thread,
) {
    /** Whether the time is over */
    @Volatile
    var expired = false
        private set

    // Whether the work ended: its thread is no longer to be interrupted
    private var ended = false

    private fun expire() =
        synchronized(this) {
            if (!ended) {
                expired = true
                thread.interrupt()
            }
        }

    private fun end() =
        synchronized(this) {
            ended = true
            // the interruption of the deadline is over with its work
            if (expired) Thread.interrupted()
        }

    companion object {
        /** [work], on the current thread, within [timeout] */
        fun <T> within(
            timeout: Duration,
            work: (Deadline) -> T,
        ): T {
            val deadline = Deadline(Thread.currentThread())
            val alarm = Solving.timer.schedule(deadline::expire, timeout.inWholeMilliseconds, TimeUnit.MILLISECONDS)
            return try {
                work(deadline)
            } finally {
                alarm.cancel(false)
                deadline.end()
            }
        }

        /**
         * [work], on the current thread, within [timeout]: a [TimedOut] if it runs out of it, whether it fails then or
         * ends (a work interrupted may end as if it found nothing: the settings of a solver stopped read as none), unless
         * what it ends with is [answered], what it found until then
         */
        fun <T> timed(
            timeout: Duration,
            answered: (T) -> Boolean = { false },
            work: (Deadline) -> T,
        ): T =
            within(timeout) { deadline ->
                val done =
                    try {
                        work(deadline)
                    } catch (failure: Exception) {
                        throw if (deadline.expired) TimedOut(timeout) else failure
                    }
                if (deadline.expired && !answered(done)) throw TimedOut(timeout)
                done
            }
    }
}
