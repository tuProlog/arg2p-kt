package it.unibo.tuprolog.argumentation.core.libs.basic

actual object EngineInterface : EngineInterfaceBase() {
    override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.ArgumentationEngineInterface.theoryCode
}
