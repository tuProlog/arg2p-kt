package it.unibo.tuprolog.argumentation.core.libs.graph

import it.unibo.tuprolog.argumentation.core.TheoryUtils
import it.unibo.tuprolog.theory.Theory

actual object AbstractMode: AbstractModeBase() {
    override val prologTheory: Theory
        get() = TheoryUtils.loadTheoryFromPrologFile("core/abstractMode")
}
