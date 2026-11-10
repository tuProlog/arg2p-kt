package it.unibo.tuprolog.argumentation.parallel.host

import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi
import java.util.concurrent.locks.ReentrantLock
import kotlin.concurrent.withLock

/**
 * The inbox of a thread of the JVM and its writer, on a lock. A stop ([interrupt]) interrupts the thread: the resolution it
 * runs halts at its next step (the machine reads the interruption of its thread), and so do those it starts, until it
 * reads the frame of the stop ([stopTakenUp]). Each stop is counted, as the frame may be read before the interruption
 * comes or after: the thread stays interrupted only while a stop it has not read is pending.
 */
@InternalArg2pApi
class ThreadInbox :
    Inbox,
    HostedThread {
    private val lock = ReentrantLock()
    private val woken = lock.newCondition()
    private val frames = ArrayDeque<String>()

    // The stops raised, less those whose frame the thread read
    private var pending = 0

    /** The thread reading the inbox, set before it starts */
    var thread: Thread? = null

    override fun put(frame: String) =
        lock.withLock {
            frames.addLast(frame)
            woken.signalAll()
        }

    // (an interruption does not end the wait: the thread stays interrupted until it reads the frame of the stop)
    override fun take(): String =
        lock.withLock {
            while (frames.isEmpty()) woken.awaitUninterruptibly()
            frames.removeFirst()
        }

    override fun interrupt() =
        lock.withLock {
            pending++
            if (pending > 0) thread?.interrupt()
        }

    // (on the thread reading the inbox: its interruption cleared once no stop is pending)
    override fun stopTakenUp() =
        lock.withLock {
            pending--
            if (pending <= 0) Thread.interrupted()
        }
}
