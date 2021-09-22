package it.unibo.tuprolog.argumentation.core.libs.structured

import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

actual object StructuredMode: StructuredModeBase() {
    override val prologTheory: Theory
        get() = Theory.parse(it.unibo.tuprolog.argumentation.core.libs.sources.QueryMode.theoryCode)
}
