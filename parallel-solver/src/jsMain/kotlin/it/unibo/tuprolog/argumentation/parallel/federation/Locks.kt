package it.unibo.tuprolog.argumentation.parallel.federation

// A JS thread shares no memory with the others: nothing to lock
internal actual fun <T> locked(
    lock: Any,
    block: () -> T,
): T = block()
