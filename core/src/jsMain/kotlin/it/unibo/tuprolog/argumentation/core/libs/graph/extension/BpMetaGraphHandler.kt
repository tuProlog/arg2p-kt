package it.unibo.tuprolog.argumentation.core.libs.graph.extension

actual object BpMetaGraphHandler : BpMetaGraphHandlerBase() {
    override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.Bp.theoryCode
}
