package it.unibo.tuprolog.argumentation.engine.words

import it.unibo.tuprolog.argumentation.InternalArg2pApi

@InternalArg2pApi
actual fun <T> locked(
    lock: Any,
    block: () -> T,
): T = synchronized(lock, block)
