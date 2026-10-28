package it.unibo.tuprolog.argumentation.parallel.federation

internal actual fun <T> locked(
    lock: Any,
    block: () -> T,
): T = synchronized(lock, block)
