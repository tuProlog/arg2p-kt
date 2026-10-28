package it.unibo.tuprolog.argumentation.core.model.terms

import it.unibo.tuprolog.argumentation.InternalArg2pApi
import it.unibo.tuprolog.argumentation.Value
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.TermFormatter
import it.unibo.tuprolog.core.operators.OperatorSet

/**
 * The terms of tuProlog as the values of arg2p ([Value]) and as text, written with the operators of arg2p: the
 * passage between the API of arg2p and tuProlog
 */
@InternalArg2pApi
object TermValues {
    /** The operators of the theories of arg2p, and the default ones */
    val operators: OperatorSet by lazy { Arg2pSolver.default().operators() }

    private val formatter by lazy { TermFormatter.prettyExpressions(operators) }

    // a prefix negation as arg2p writes it (-a, ~a), rather than - a: at the start, or after an opening or a separator
    private val negation = Regex("""(^|[(\[,|]\s*)([-~]) (?=\S)""")

    /** [term] as Prolog text, written with the operators of arg2p */
    fun format(term: Term): String = negation.replace(formatter.format(term), "$1$2")

    /** The value of [term] */
    fun of(term: Term): Value =
        when {
            term.isVar -> Value.Variable(term.castToVar().name)
            term.isInteger ->
                term.castToInteger().value.let { value ->
                    runCatching { Value.Integer(value.toIntExact()) }.getOrElse { Value.Real(value.toDouble()) }
                }
            term.isReal -> Value.Real(term.castToReal().value.toDouble())
            term.isList && term.castToList().isWellFormed -> Value.List(term.castToList().toList().map { of(it) })
            term.isAtom -> Value.Atom(term.castToAtom().value)
            else -> term.castToStruct().let { struct -> Value.Struct(struct.functor, struct.args.map { of(it) }) }
        }
}
