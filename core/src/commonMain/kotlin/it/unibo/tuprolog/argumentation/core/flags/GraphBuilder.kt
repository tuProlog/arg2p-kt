package it.unibo.tuprolog.argumentation.core.flags

/**
 * The module building the argumentation graph: from the rules of the theory (ruleGraph), or from its arguments and
 * attacks (abstractGraph). The profile of the theory sets it (see graphBuildMode); in the profile free, the solver
 * and the theory do.
 */
object GraphBuilder : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "graphBuilder"

    override fun default(): String = "ruleGraph"

    override fun values(): Iterable<String> = listOf("ruleGraph", "abstractGraph")
}
