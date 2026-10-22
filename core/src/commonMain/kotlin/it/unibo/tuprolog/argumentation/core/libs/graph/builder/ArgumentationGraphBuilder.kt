package it.unibo.tuprolog.argumentation.core.libs.graph.builder

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

abstract class ArgumentationGraphBuilderBase : PrologModule("prolog.argumentation.graph", "standard_af")

expect object ArgumentationGraphBuilder : ArgumentationGraphBuilderBase {
    override val prologRawTheory: String
}
