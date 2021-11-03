package it.unibo.tuprolog.argumentation.core.libs.graph

actual object AbstractMode : AbstractModeBase() {
    override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.AbstractMode.theoryCode
}
