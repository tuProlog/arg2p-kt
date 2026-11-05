package it.unibo.tuprolog.argumentation.core.engine.vm

internal actual fun resolutionInterrupted(): Boolean = Thread.currentThread().isInterrupted
