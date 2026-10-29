package it.unibo.tuprolog.argumentation.core.engine.fsm

// On the JVM, a resolution runs on a thread of its own caller, which interrupts it to stop it (a timeout, a Stop)
internal actual fun resolutionInterrupted(): Boolean = Thread.currentThread().isInterrupted
