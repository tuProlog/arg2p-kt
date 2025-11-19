package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

actual object BpLabeller : BpLabellerBase() {
    actual override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.BpGrounded.theoryCode
}
