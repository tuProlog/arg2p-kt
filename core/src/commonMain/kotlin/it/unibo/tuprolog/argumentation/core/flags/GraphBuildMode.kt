package it.unibo.tuprolog.argumentation.core.flags

object GraphBuildMode : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "graphBuildMode"

    override fun default(): String = "standard_af"

    override fun values(): Iterable<String> = listOf("standard_af")
}
