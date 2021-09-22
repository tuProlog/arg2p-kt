package it.unibo.tuprolog.argumentation.core.libs

import it.unibo.tuprolog.argumentation.core.TheoryUtils

actual object EngineInterface : EngineInterfaceBase() {
    override val prologRawTheory: String
        get() = TheoryUtils.loadTheoryFromPrologFile("argumentationEngineInterface")
}
