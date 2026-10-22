package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

abstract class CompleteLabellerBase : PrologModule("prolog.argumentation.graph.labelling.complete", "complete")

expect object CompleteLabeller : CompleteLabellerBase {
    override val prologRawTheory: String
}
