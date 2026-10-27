package it.unibo.tuprolog.argumentation.core.flags

object GraphExtension : ArgsFlag<Iterable<String>, Iterable<String>> {
    override fun predicate(): String = "graphExtension"

    override fun default(): Iterable<String> = listOf("standardPref")

    override fun values(): Iterable<String> =
        listOf("rebutRestriction", "bp", "standardPref", "defeasiblePref", "defeasibleAllPref", "abaPref")
}
