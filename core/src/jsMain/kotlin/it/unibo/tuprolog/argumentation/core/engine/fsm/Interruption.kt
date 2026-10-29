package it.unibo.tuprolog.argumentation.core.engine.fsm

// In JavaScript, a resolution is never interrupted: a Web Worker is stopped by terminating it
internal actual fun resolutionInterrupted(): Boolean = false
