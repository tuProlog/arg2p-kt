package it.unibo.tuprolog.argumentation.core.flags

/**
 * How a query is answered for a semantics with several labellings: the statements labelled in each labelling, one answer
 * for each (enumerate), or across them all (credulous, skeptical; see the module structuredPartial)
 */
object AnswerMode : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "answerMode"

    override fun default(): String = "enumerate"

    override fun values(): Iterable<String> = listOf("enumerate", "credulous", "skeptical")
}
