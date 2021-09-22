package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

actual object StatementLabeller : StatementLabellerBase() {
    override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.StatementLabelling.theoryCode
}
