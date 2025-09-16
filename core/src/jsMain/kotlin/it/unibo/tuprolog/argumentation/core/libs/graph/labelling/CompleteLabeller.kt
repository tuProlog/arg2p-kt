package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

actual object CompleteLabeller : CompleteLabellerBase() {
    actual override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.Complete.theoryCode
}
