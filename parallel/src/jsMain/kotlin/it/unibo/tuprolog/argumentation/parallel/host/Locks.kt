package it.unibo.tuprolog.argumentation.parallel.host

import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi

// A JS thread shares no memory with the others: nothing to lock
@InternalArg2pApi
actual fun <T> locked(
    lock: Any,
    block: () -> T,
): T = block()
