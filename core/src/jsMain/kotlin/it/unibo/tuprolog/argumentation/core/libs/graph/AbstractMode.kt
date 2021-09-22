package it.unibo.tuprolog.argumentation.core.libs.graph

import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

actual object AbstractMode: AbstractModeBase() {
    override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.AbstractMode.theoryCode
}
