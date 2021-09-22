package it.unibo.tuprolog.argumentation.core.libs.graph

import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

actual object AbstractMode: AbstractModeBase() {
    override val prologTheory: Theory
        get() = Theory.parse(it.unibo.tuprolog.argumentation.core.libs.sources.AbstractMode.theoryCode)
}
