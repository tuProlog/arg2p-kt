package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.TheoryUtils

actual object CompleteLabeller : CompleteLabellerBase() {
    override val prologRawTheory: String
        get() = TheoryUtils.loadTheoryFromPrologFile("core/labellings/argument/complete")
}
