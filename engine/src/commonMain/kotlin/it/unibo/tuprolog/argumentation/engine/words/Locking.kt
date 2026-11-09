package it.unibo.tuprolog.argumentation.engine.words

import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi

/** [block] run while holding [lock], where threads exist (the JVM); run as it is elsewhere */
@InternalArg2pApi
expect fun <T> locked(
    lock: Any,
    block: () -> T,
): T
