package it.unibo.tuprolog.argumentation.core.model.terms

import it.unibo.tuprolog.argumentation.InternalArg2pApi
import it.unibo.tuprolog.argumentation.Value
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.TermFormatter
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.parsing.parse

/**
 * The terms of tuProlog as the values of arg2p ([Value]) and as text, written with the operators of arg2p: the
 * passage between the API of arg2p and tuProlog
 */
@InternalArg2pApi
object TermValues {
    /** The operators of the theories of arg2p, and the default ones */
    val operators: OperatorSet by lazy { Arg2pSolver.default().operators() }

    private val formatter by lazy { TermFormatter.prettyExpressions(operators) }

    // a prefix negation as arg2p writes it (-a, ~a), rather than - a: at the start, or after an opening or a separator;
    // a quoted atom or string is matched whole, to be left as it is
    private val negation = Regex("""('(?:[^'\\]|\\.)*'|"(?:[^"\\]|\\.)*")|(^|[(\[,|]\s*)([-~]) (?=\S)""")

    /** The term written in [text], a goal or a value (its final dot left out), read with [operators] */
    fun term(
        text: String,
        operators: OperatorSet = this.operators,
    ): Term = Term.parse(text.trim().removeSuffix("."), operators)

    /** [term] as Prolog text, written with the operators of arg2p */
    fun format(term: Term): String =
        negation.replace(formatter.format(term)) { match ->
            match.groups[1]?.value ?: (match.groupValues[2] + match.groupValues[3])
        }

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
