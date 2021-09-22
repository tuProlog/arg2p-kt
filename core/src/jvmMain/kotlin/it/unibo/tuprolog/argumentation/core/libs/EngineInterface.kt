package it.unibo.tuprolog.argumentation.core.libs

import it.unibo.tuprolog.argumentation.core.TheoryUtils
import it.unibo.tuprolog.theory.Theory

actual object EngineInterface: EngineInterfaceBase() {
    override val prologTheory: Theory
        get() = TheoryUtils.loadTheoryFromPrologFile("argumentationEngineInterface")
}