package it.unibo.tuprolog.argumentation.parallel.federation

/**
 * What [block] does, a failure of it reported (see [report]) rather than raised: a frame, a beacon or a task whose
 * handling fails is skipped, as an actor resumed on a failure does, and the transport goes on
 */
internal fun handled(block: () -> Unit) {
    runCatching(block).onFailure(::report)
}

/** [failure] reported where the platform reports errors: the standard error of the JVM, the console of JavaScript */
internal expect fun report(failure: Throwable)
