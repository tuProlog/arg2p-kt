package it.unibo.tuprolog.argumentation.core.libs.graph

import it.unibo.tuprolog.argumentation.core.libs.*
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library

sealed class AbstractModeBase : ArgLibrary, RawPrologContent, Loadable {
    override val baseContent: AliasedLibrary
        get() = Library.aliased(
            alias = "prolog.argumentation.abstract",
            theory = this.prologTheory
        )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(GraphExtension, ArgumentLabellingMode, StatementLabellingMode, GraphBuildMode)

    override fun identifier(): String = "abstract"
}

expect object AbstractMode : AbstractModeBase

object GraphExtension : ArgsFlag<Iterable<String>, Iterable<String>> {
    override fun predicate(): String = "graphExtension"
    override fun default(): Iterable<String> = listOf("standardPref")
    override fun values(): Iterable<String> = listOf("rebutRestriction", "bp", "standardPref", "defeasiblePref", "defeasibleAllPref")
}

object ArgumentLabellingMode : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "argumentLabellingMode"
    override fun default(): String = "grounded"
    override fun values(): Iterable<String> = listOf(
        "grounded",
        "complete",
        "bp_grounded",
        "bp_grounded_partial",
        "bp_grounded_complete"
    )
}

object StatementLabellingMode : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "statementLabellingMode"
    override fun default(): String = "base"
    override fun values(): Iterable<String> = listOf("base")
}

object GraphBuildMode : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "graphBuildMode"
    override fun default(): String = "base"
    override fun values(): Iterable<String> = listOf("base")
}
