package it.unibo.tuprolog.argumentation.core.libs.utils

import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

actual object Debug: DebugBase() {
    override val prologTheory: Theory
        get() = Theory.parse(it.unibo.tuprolog.argumentation.core.libs.sources.Debug.theoryCode)
}
