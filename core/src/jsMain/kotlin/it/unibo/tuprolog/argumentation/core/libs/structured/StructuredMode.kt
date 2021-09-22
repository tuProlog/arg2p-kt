package it.unibo.tuprolog.argumentation.core.libs.structured

import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

actual object StructuredMode: StructuredModeBase() {
    override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.QueryMode.theoryCode
}
