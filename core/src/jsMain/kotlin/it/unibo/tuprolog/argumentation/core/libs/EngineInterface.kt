package it.unibo.tuprolog.argumentation.core.libs

import it.unibo.tuprolog.argumentation.core.libs.sources.ArgumentationEngineInterface

actual object EngineInterface : EngineInterfaceBase() {
    override val prologRawTheory: String
        get() = ArgumentationEngineInterface.theoryCode
}
