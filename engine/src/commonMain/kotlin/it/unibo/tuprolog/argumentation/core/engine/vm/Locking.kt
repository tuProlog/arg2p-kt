package it.unibo.tuprolog.argumentation.core.engine.vm

/** [block] run while holding [lock], where threads exist (the JVM); run as it is elsewhere */
internal expect fun <T> locked(
    lock: Any,
    block: () -> T,
): T
