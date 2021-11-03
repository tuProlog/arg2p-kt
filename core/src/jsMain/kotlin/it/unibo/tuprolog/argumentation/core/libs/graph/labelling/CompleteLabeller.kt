package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

actual object CompleteLabeller : CompleteLabellerBase() {
    override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.Complete.theoryCode
}
