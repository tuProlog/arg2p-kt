package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.argumentation.core.libraries.LibraryRegistry
import it.unibo.tuprolog.argumentation.core.libraries.framework.loader.notation.ModuleNotation
import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi
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
    /**
     * The operators of the theories of arg2p, and the default ones: those of the modules, and the notations of all the
     * libraries (the rules, the abstract frameworks, ...), as a solver reads a theory with the notations of its own
     */
    val operators: OperatorSet by lazy { LibraryRegistry.all.fold(ModuleNotation.theories) { all, library -> all + library.notation } }

    private val formatter by lazy { TermFormatter.prettyExpressions(operators) }

    /** The term written in [text], a goal or a value (its final dot left out), read with [operators] */
    fun term(
        text: String,
        operators: OperatorSet = this.operators,
    ): Term = Term.parse(text.trim().removeSuffix("."), operators)

    /**
     * [term] as Prolog text, written with the operators of arg2p as tuProlog writes them: a prefix operator apart from its
     * argument (- a, ~ a), which reads back as the same term in every case (-1 would be a number, - 1 is -(1))
     */
    fun format(term: Term): String = formatter.format(term)

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
