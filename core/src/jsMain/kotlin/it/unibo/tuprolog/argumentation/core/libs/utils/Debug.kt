package it.unibo.tuprolog.argumentation.core.libs.utils

actual object Debug : DebugBase() {
    override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.Debug.theoryCode
}
