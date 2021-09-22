package it.unibo.tuprolog.argumentation.core.libs.graph.builder

import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

actual object ArgumentationGraphBuilder: ArgumentationGraphBuilderBase() {
    override val prologTheory: Theory
        get() = Theory.parse(it.unibo.tuprolog.argumentation.core.libs.sources.ArgumentationGraph.theoryCode)
}
