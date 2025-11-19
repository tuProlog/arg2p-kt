package it.unibo.tuprolog.argumentation.core.libs.graph

actual object AbstractMode : AbstractModeBase() {
    actual override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.AbstractMode.theoryCode
}
