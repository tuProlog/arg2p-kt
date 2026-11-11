package it.unibo.tuprolog.argumentation.core.libraries.language.abstractLanguage

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.abstractLanguage.prolog.THEORY
import it.unibo.tuprolog.core.operators.OperatorSet

/**
 * The language of Dung frameworks (module `abstractLanguage`, the graphLanguage of the profile abstract): the arguments a
 * theory states or derives, `a.` or arg(a), and the attacks, `a -> b.`, `a <-> b.` for a mutual one, or att(a, b), facts or
 * Prolog clauses (`X -> Y :- beats(X, Y).`), read as data (see parser::theoryClause/2): arguments/1 and attacks/1, which the
 * module abstractGraph builds the graph from. Any other clause is data for these.
 */
internal object AbstractFrameworkLanguageLibrary : PrologLibrary("abstractLanguage") {
    // (its own theory reads the clauses written with the notation)
    override val theoryOperators: OperatorSet
        get() = super.theoryOperators + AbstractNotation.operators

    override val notation: OperatorSet
        get() = AbstractNotation.operators

    override val prologRawTheory: String = THEORY
}
