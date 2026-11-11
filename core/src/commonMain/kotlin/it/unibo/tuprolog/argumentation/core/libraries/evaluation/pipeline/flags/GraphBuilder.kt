package it.unibo.tuprolog.argumentation.core.libraries.evaluation.pipeline.flags

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag
import it.unibo.tuprolog.argumentation.GraphBuilder as GraphBuilderValue

/**
 * The module building the argumentation graph: from the rules of the theory (ruleGraph), or from its arguments and
 * attacks (abstractGraph). The profile of the theory sets it (see graphBuildMode); in the profile free, the solver
 * and the theory do.
 */
object GraphBuilder : ArgsFlag {
    override fun predicate(): String = "graphBuilder"

    override fun default(): String = GraphBuilderValue.RULE_GRAPH.value

    override val isOpen: Boolean = true

    override fun values(): Iterable<String> = GraphBuilderValue.entries.map { it.value }
}
