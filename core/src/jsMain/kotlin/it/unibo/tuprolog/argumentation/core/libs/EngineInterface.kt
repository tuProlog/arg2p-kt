package it.unibo.tuprolog.argumentation.core.libs

import it.unibo.tuprolog.argumentation.core.libs.sources.ArgumentationEngineInterface
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

actual object EngineInterface: EngineInterfaceBase() {
    override val prologTheory: Theory
        get() = Theory.parse(ArgumentationEngineInterface.theoryCode)
}