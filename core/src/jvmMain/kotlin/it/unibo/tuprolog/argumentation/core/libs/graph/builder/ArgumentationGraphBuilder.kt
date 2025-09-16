package it.unibo.tuprolog.argumentation.core.libs.graph.builder

import it.unibo.tuprolog.argumentation.core.TheoryUtils

actual object ArgumentationGraphBuilder : ArgumentationGraphBuilderBase() {
    actual override val prologRawTheory: String
        get() = TheoryUtils.loadTheoryFromPrologFile("core/graph/argumentationGraph")
}
