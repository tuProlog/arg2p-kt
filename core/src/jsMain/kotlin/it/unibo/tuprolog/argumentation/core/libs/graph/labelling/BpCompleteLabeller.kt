package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

actual object BpCompleteLabeller : BpCompleteLabellerBase() {
    override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.BpGroundedComplete.theoryCode
}
