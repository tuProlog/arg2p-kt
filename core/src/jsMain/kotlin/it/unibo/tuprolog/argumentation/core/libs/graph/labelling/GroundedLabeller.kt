package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

actual object GroundedLabeller : GroundedLabellerBase() {
    actual override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.Grounded.theoryCode
}
