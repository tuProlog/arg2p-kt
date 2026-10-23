package it.unibo.tuprolog.argumentation.core.flags

object ArgumentLabellingMode : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "argumentLabellingMode"

    override fun default(): String = "grounded"

    override fun values(): Iterable<String> =
        listOf(
            "conflictfree",
            "admissible",
            "stronglyadmissible",
            "grounded",
            "complete",
            "preferred",
            "semistable",
            "stable",
            "ideal",
            "eager",
            "naive",
            "stage",
            "cf2",
            "stage2",
            "bp_grounded",
            "bp_grounded_partial",
            "bp_grounded_complete",
            "grounded_old",
        )
}
