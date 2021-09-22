package it.unibo.tuprolog.argumentation.core.libs.graph.builder

import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

actual object ArgumentationGraphBuilder: ArgumentationGraphBuilderBase() {
    override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.ArgumentationGraph.theoryCode
}
