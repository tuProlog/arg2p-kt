package it.unibo.tuprolog.argumentation.parallel.host

import it.unibo.tuprolog.argumentation.ExperimentalArg2pApi
import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi

/**
 * The frames a thread that may wait reads, one at a time, as text. On the JVM a queue on a lock (ThreadInbox); in
 * JavaScript a SharedArrayBuffer read with Atomics.wait (a thread waiting there handles no message): how a thread waits is
 * all that changes from a platform to another.
 */
@InternalArg2pApi
@ExperimentalArg2pApi
fun interface Inbox {
    /** The next frame, waited for */
    fun take(): String

    /**
     * The frame of a stop read (the Stop of the clients for the solver, an Abandon for a consumer): what the thread is told
     * after it runs, unless another stop comes (see [HostedThread.interrupt])
     */
    fun stopTakenUp() = Unit
}

/** A thread that may wait, as what never does sees it: [put] writes a frame to its inbox */
@InternalArg2pApi
@ExperimentalArg2pApi
fun interface HostedThread {
    fun put(frame: String)

    /**
     * The resolution the thread runs halted at its next step, and those of what it was told before, until it reads the frame
     * of the stop, put next ([Inbox.stopTakenUp]): where the platform lets another thread tell it (a worker of JavaScript, a
     * count on shared memory, see SharedInbox); nothing otherwise
     */
    fun interrupt() = Unit
}

/**
 * The frames of a thread, read from its [inbox]: the next one, or the one a call waits for, those before it kept for
 * after (the thread reads them alone, one at a time)
 */
internal class Mailbox(
    private val inbox: Inbox,
) {
    private val kept = ArrayDeque<HostFrame>()

    /** The next frame not waited for */
    fun next(): HostFrame = kept.removeFirstOrNull() ?: taken()

    /** The frame [wanted] is, waited for */
    fun await(wanted: (HostFrame) -> Boolean): HostFrame {
        kept.firstOrNull(wanted)?.let {
            kept.remove(it)
            return it
        }
        while (true) {
            val frame = taken()
            if (wanted(frame)) return frame
            // (what is told to a consumer no longer waiting is left)
            if (frame !is HostFrame.Answer && frame !is HostFrame.Abandon) kept.addLast(frame)
        }
    }

    // The next frame of the inbox: an Abandon follows the stop of the consumer's thread (see ConsumerThreads.abandon)
    private fun taken(): HostFrame = HostCodec.decode(inbox.take()).also { if (it is HostFrame.Abandon) inbox.stopTakenUp() }
}

/**
 * The threads of a node as a host: each runs one consumer at a time (see runConsumerThread), its frames given back to
 * [received]. A consumer waits for the others of its graph to end, so it never queues behind another: with no thread free
 * one more is made by [more], up to [MOST_THREADS] times the [offered] ones (the node may be running a graph of its own and
 * lending to several at once); past them the graph fails, rather than waiting. [ready] are the threads there from the
 * start: making one may take long.
 */
@InternalArg2pApi
@ExperimentalArg2pApi
class ConsumerThreads(
    override val id: String,
    private val offered: Int,
    ready: List<HostedThread> = emptyList(),
    private val more: () -> HostedThread,
) : ConsumerHost {
    private class Running(
        val thread: HostedThread,
        val toStore: (HostFrame.Call) -> Unit,
    )

    private val lock = Any()
    private val threads = ready.toMutableList()
    private val busy = HashSet<HostedThread>()
    private val running = HashMap<Pair<String, Int>, Running>()

    override val capacity: Int get() = offered

    override fun start(
        start: HostFrame.Start,
        toStore: (HostFrame.Call) -> Unit,
    ) {
        val thread =
            locked(lock) {
                free()?.also {
                    busy += it
                    running[start.session to start.consumer] = Running(it, toStore)
                }
            }
        if (thread == null) {
            return toStore(HostFrame.Call.failing(start.session, start.consumer, "the node $id has no thread free for a consumer"))
        }
        thread.put(HostCodec.encode(start))
    }

    // (under the lock) a thread with no consumer, made if none is and one can be
    private fun free(): HostedThread? =
        threads.firstOrNull { it !in busy }
            ?: more.takeIf { threads.size < offered * MOST_THREADS }?.invoke()?.also { threads += it }

    override fun answer(answer: HostFrame.Answer) {
        val running = locked(lock) { running[answer.session to answer.consumer] } ?: return
        running.thread.put(HostCodec.encode(answer))
    }

    // (the resolution of the consumer interrupted where the platform lets it be: one that never reads its store again would
    // not read that it was abandoned)
    override fun abandon(abandon: HostFrame.Abandon) {
        val running = locked(lock) { running[abandon.session to abandon.consumer] } ?: return
        running.thread.put(HostCodec.encode(abandon))
        running.thread.interrupt()
    }

    /** [frame], from the thread [from]: what its consumer asks of its store, or that it ended */
    fun received(
        from: HostedThread,
        frame: HostFrame,
    ) {
        when (frame) {
            is HostFrame.Call -> locked(lock) { running[frame.session to frame.consumer] }?.toStore?.invoke(frame)
            HostFrame.Done ->
                locked(lock) {
                    busy -= from
                    running.entries.removeAll { it.value.thread === from }
                }
            else -> Unit
        }
    }

    /** The consumers given up, and the loops of the threads ended */
    fun close() {
        val (consumers, all) = locked(lock) { running.toMap() to threads.toList() }
        consumers.forEach { (key, consumer) ->
            consumer.thread.put(HostCodec.encode(HostFrame.Abandon(key.first, key.second, "The solver is closed")))
            consumer.thread.interrupt()
        }
        all.forEach { it.put(HostCodec.encode(HostFrame.Stop)) }
    }

    private companion object {
        const val MOST_THREADS = 4
    }
}
