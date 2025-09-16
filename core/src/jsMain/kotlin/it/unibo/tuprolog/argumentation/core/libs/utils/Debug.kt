package it.unibo.tuprolog.argumentation.core.libs.utils

actual object Debug : DebugBase() {
    actual override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.Debug.theoryCode
}
