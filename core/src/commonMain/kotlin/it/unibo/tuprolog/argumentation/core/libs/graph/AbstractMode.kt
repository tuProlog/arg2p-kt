package it.unibo.tuprolog.argumentation.core.libs.graph

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.LazyRawPrologContent
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.basic.DynamicLoader
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.library.Library

abstract class AbstractModeBase :
    LazyRawPrologContent(),
    ArgLibrary,
    Loadable {
    override val alias = "prolog.argumentation.abstract"

    override val baseContent: Library
        get() =
            Library.of(
                alias = this.alias,
                clauses = this.prologTheory,
            )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(GraphExtension, ArgumentLabellingMode, StatementLabellingMode, GraphBuildMode)

    override fun identifier(): String = "abstract"

    override val theoryOperators =
        DynamicLoader
            .operators()
            .plus(OperatorSet.DEFAULT)
}

expect object AbstractMode : AbstractModeBase {
    override val prologRawTheory: String
}

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
            "grounded",
            "complete",
            "preferred",
            "semistable",
            "stable",
            "ideal",
            "eager",
            "naive",
            "stage",
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
