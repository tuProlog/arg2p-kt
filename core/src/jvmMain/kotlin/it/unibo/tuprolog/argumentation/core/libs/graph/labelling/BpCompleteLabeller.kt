package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.TheoryUtils

actual object BpCompleteLabeller : BpCompleteLabellerBase() {
    override val prologRawTheory: String
        get() = TheoryUtils.loadTheoryFromPrologFile("core/labellings/argument/bpGroundedComplete")
}
