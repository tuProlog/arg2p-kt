package it.unibo.tuprolog.argumentation.parallel.store

internal actual class Waiting actual constructor() {
    private val lock =
        java.util.concurrent.locks
            .ReentrantLock()
    private val woken = lock.newCondition()

    actual fun <T> locked(block: () -> T): T {
        lock.lock()
        try {
            return block()
        } finally {
            lock.unlock()
        }
    }

    actual fun await() = woken.await()

    actual fun wakeAll() = woken.signalAll()
}
