package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

actual object BpPartialLabeller : BpPartialLabellerBase() {
    override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.BpGroundedPartial.theoryCode
}
