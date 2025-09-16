package it.unibo.tuprolog.argumentation.core.libs.basic

import it.unibo.tuprolog.argumentation.core.TheoryUtils

actual object EngineInterface : EngineInterfaceBase() {
    actual override val prologRawTheory: String
        get() = TheoryUtils.loadTheoryFromPrologFile("argumentationEngineInterface")
}
