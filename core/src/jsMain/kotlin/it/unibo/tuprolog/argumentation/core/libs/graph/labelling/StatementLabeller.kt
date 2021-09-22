package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

actual object StatementLabeller: StatementLabellerBase() {
    override val prologTheory: Theory
        get() = Theory.parse(it.unibo.tuprolog.argumentation.core.libs.sources.StatementLabelling.theoryCode)
}
