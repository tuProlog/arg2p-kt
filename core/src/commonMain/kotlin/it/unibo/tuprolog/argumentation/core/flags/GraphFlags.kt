package it.unibo.tuprolog.argumentation.core.flags

import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag

object GraphExtension : ArgsFlag<Iterable<String>, Iterable<String>> {
    override fun predicate(): String = "graphExtension"

    override fun default(): Iterable<String> = listOf("standardPref")

    override fun values(): Iterable<String> = listOf("rebutRestriction", "bp", "standardPref", "defeasiblePref", "defeasibleAllPref")
}

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

object StatementLabellingMode : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "statementLabellingMode"

    override fun default(): String = "statement"

    override fun values(): Iterable<String> = listOf("statement", "statement_pass_through", "statement_binary")
}

object GraphBuildMode : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "graphBuildMode"

    override fun default(): String = "standard_af"

    override fun values(): Iterable<String> = listOf("standard_af")
}
