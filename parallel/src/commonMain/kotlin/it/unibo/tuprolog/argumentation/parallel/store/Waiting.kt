package it.unibo.tuprolog.argumentation.parallel.store

/**
 * A lock threads wait on until another wakes them, where threads exist (the JVM). Elsewhere a block runs as it is, and
 * waiting is an error: no other thread could wake the one waiting.
 */
internal expect class Waiting() {
    fun <T> locked(block: () -> T): T

    /** Waits, the lock released, until woken (within [locked]) */
    fun await()

    /** Wakes the threads waiting (within [locked]) */
    fun wakeAll()
}
