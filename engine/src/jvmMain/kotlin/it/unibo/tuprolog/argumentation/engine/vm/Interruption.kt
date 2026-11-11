package it.unibo.tuprolog.argumentation.engine.vm

internal actual fun resolutionInterrupted(): Boolean = Thread.currentThread().isInterrupted
