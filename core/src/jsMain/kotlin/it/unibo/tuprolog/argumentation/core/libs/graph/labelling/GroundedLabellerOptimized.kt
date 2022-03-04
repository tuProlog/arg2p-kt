package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

actual object GroundedLabellerOptimized : GroundedLabellerOptimizedBase() {
    override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.GroundedOptimized.theoryCode
}
