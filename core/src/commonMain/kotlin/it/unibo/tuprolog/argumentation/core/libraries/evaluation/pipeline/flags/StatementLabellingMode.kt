package it.unibo.tuprolog.argumentation.core.libraries.evaluation.pipeline.flags

import it.unibo.tuprolog.argumentation.StatementLabelling
import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag

object StatementLabellingMode : ArgsFlag {
    override fun predicate(): String = "statementLabellingMode"

    override fun default(): String = StatementLabelling.STATEMENT.value

    override val isOpen: Boolean = true

    override fun values(): Iterable<String> = StatementLabelling.entries.map { it.value }
}
