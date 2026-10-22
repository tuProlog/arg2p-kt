@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.core.js

import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Real
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.TermFormatter
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import kotlin.js.JsExport
import it.unibo.tuprolog.core.List as LogicList

/**
 * The terms of tuProlog for JS: their factories, which JS cannot reach (tuProlog exports none of its declarations),
 * parsing and formatting. The terms are tuProlog's own: JS reads them through their named methods (castToStruct,
 * getArgAt, get_functor, get_isVar, ...).
 */
@JsExport
object JsTerms {
    /** The term written in [text], with the operators of arg2p theories (see [JsArg2p.operators]) */
    fun parse(text: String): Term = Term.parse(text, JsArg2p.operators)

    /** The term written in [text], with the given operators */
    fun parseWith(
        text: String,
        operators: OperatorSet,
    ): Term = Term.parse(text, operators)

    fun atom(name: String): Atom = Atom.of(name)

    fun variable(name: String): Var = Var.of(name)

    fun integer(value: Int): Integer = Integer.of(value)

    fun real(value: Double): Real = Real.of(value)

    fun struct(
        functor: String,
        args: Array<Term>,
    ): Struct = Struct.of(functor, args.toList())

    fun list(items: Array<Term>): Term = LogicList.of(items.toList())

    // Reading terms: tuProlog's properties (isVar, functor, ...) are compiled to names JS cannot know

    /** What the term is: variable, integer, real, atom, list or struct */
    fun kind(term: Term): String =
        when {
            term.isVar -> "variable"
            term.isInteger -> "integer"
            term.isReal -> "real"
            term.isList -> "list"
            term.isAtom -> "atom"
            else -> "struct"
        }

    /** The functor of a compound term, or the name of an atom */
    fun functor(term: Term): String = term.castToStruct().functor

    /** The number of the arguments of a compound term (0 for an atom) */
    fun arity(term: Term): Int = term.castToStruct().arity

    /** The name of a variable */
    fun name(variable: Term): String = variable.castToVar().name

    /** The value of a number */
    fun numberValue(number: Term): Double = number.castToNumeric().decimalValue.toDouble()

    /** The items of a list term */
    fun items(list: Term): Array<Term> = list.castToList().toList().toTypedArray()

    /** The arguments of a compound term */
    fun args(struct: Term): Array<Term> = struct.castToStruct().args.toTypedArray()

    /** The term as Prolog would write it, operators included */
    fun format(term: Term): String = TermFormatter.prettyExpressions().format(term)
}

/** The theories of tuProlog for JS: parsing, and their clauses */
@JsExport
object JsTheories {
    /** The theory written in [text], with the operators of arg2p theories (see [JsArg2p.operators]) */
    fun parse(text: String): Theory = Theory.parse(text, JsArg2p.operators)

    fun clauses(theory: Theory): Array<Term> = theory.clauses.toList().toTypedArray()

    fun format(theory: Theory): String = theory.toString(asPrologText = true)
}
