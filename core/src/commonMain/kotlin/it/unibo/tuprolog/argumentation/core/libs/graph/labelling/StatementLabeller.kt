package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

abstract class StatementLabellerBase : PrologModule("prolog.argumentation.graph.labelling.statement", "statement")

expect object StatementLabeller : StatementLabellerBase {
    override val prologRawTheory: String
}
