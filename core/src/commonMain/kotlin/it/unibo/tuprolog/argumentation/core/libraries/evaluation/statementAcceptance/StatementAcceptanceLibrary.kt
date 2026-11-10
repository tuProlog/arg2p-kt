package it.unibo.tuprolog.argumentation.core.libraries.evaluation.statementAcceptance

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.statementAcceptance.flags.AnswerMode
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.statementAcceptance.prolog.THEORY

/**
 * The acceptance of a statement (module `statementAcceptance`): the labels of the statements for a goal, as answerMode states,
 * on the part of the graph they depend on (see pipeline::evaluate/1).
 */
object StatementAcceptanceLibrary : PrologLibrary("statementAcceptance") {
    override val baseFlags: Iterable<ArgsFlag>
        get() = listOf(AnswerMode)

    override val prologRawTheory: String = THEORY
}
