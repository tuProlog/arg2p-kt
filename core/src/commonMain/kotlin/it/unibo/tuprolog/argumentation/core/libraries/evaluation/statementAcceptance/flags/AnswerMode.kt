package it.unibo.tuprolog.argumentation.core.libraries.evaluation.statementAcceptance.flags

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag
import it.unibo.tuprolog.argumentation.AnswerMode as AnswerModeValue

/**
 * How a query is answered for a semantics with several labellings: the statements labelled in each labelling, one answer
 * for each (enumerate), or across them all (credulous, skeptical; see the module statementAcceptance)
 */
object AnswerMode : ArgsFlag {
    override fun predicate(): String = "answerMode"

    override fun default(): String = AnswerModeValue.ENUMERATE.value

    override fun values(): Iterable<String> = AnswerModeValue.entries.map { it.value }
}
