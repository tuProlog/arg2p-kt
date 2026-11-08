package it.unibo.tuprolog.argumentation.engine.vm

/**
 * Whether the resolution running on this thread was interrupted by its caller, which stops it: on the JVM, the
 * interruption of its thread (Thread.interrupt, Future.cancel(true)); never in JavaScript
 */
internal expect fun resolutionInterrupted(): Boolean
