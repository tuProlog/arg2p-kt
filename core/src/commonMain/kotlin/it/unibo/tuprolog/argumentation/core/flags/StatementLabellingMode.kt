package it.unibo.tuprolog.argumentation.core.flags

object StatementLabellingMode : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "statementLabellingMode"

    override fun default(): String = "statement"

    override fun values(): Iterable<String> = listOf("statement", "statement_pass_through", "statement_binary")
}
