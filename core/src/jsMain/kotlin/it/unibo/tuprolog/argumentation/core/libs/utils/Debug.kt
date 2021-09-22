package it.unibo.tuprolog.argumentation.core.libs.utils

import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

actual object Debug: DebugBase() {
    override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.Debug.theoryCode
}
