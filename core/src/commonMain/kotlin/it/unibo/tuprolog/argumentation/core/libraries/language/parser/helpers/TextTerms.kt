package it.unibo.tuprolog.argumentation.core.libraries.language.parser.helpers

import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.SyntaxProblem
import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.operators.OperatorSet

/** The term [text] reads as, with [operators]: an Arg2pException telling where it does not parse (as a query) */
@InternalArg2pApi
fun termOf(
    text: String,
    operators: OperatorSet,
): Term = SyntaxProblem.parsing("Query") { TermValues.term(text, operators) }

/** The goal [text] reads as, with [operators]: an Arg2pException if it does not parse, or is no goal */
@InternalArg2pApi
fun goalOf(
    text: String,
    operators: OperatorSet,
): Struct = termOf(text, operators).let { it as? Struct ?: throw Arg2pException("Not a goal: $text") }
