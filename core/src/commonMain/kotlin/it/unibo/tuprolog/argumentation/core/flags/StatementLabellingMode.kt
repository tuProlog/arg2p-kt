package it.unibo.tuprolog.argumentation.core.flags

import it.unibo.tuprolog.argumentation.StatementLabelling

object StatementLabellingMode : ArgsFlag {
    override fun predicate(): String = "statementLabellingMode"

    override fun default(): String = StatementLabelling.STATEMENT.value

    override val isOpen: Boolean = true

    override fun values(): Iterable<String> = StatementLabelling.entries.map { it.value }
}
