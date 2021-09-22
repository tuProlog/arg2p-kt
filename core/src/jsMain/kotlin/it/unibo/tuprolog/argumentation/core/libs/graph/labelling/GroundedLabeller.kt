package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

actual object GroundedLabeller: GroundedLabellerBase() {
    override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.Grounded.theoryCode
}
