package it.unibo.tuprolog.argumentation.core.engine.vm

internal actual fun <T> locked(
    lock: Any,
    block: () -> T,
): T = synchronized(lock, block)
