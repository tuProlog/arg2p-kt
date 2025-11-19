package it.unibo.tuprolog.argumentation.core.libs.graph.builder

actual object ArgumentationGraphBuilder : ArgumentationGraphBuilderBase() {
    actual override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.ArgumentationGraph.theoryCode
}
