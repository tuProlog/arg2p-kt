package it.unibo.tuprolog.argumentation.core.libraries.language.ruleNotation

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.ruleNotation.notation.RuleNotation
import it.unibo.tuprolog.argumentation.core.libraries.language.ruleNotation.prolog.THEORY
import it.unibo.tuprolog.core.operators.OperatorSet

/**
 * The notation of the rules of arg2p (module `ruleNotation`): defeasible rules `r : a, b => c`, strict rules `r : a -> c`,
 * ordinary premises `f :=> a` and axioms `f :-> a`, read as the kinds the parser gathers (rules/1, strictRules/3, premises/1,
 * axioms/1, see parser::extension/1). The language of a theory with no graphLanguage stated; the languages of the profiles
 * (aspicLanguage, abaLanguage, defeasibleLanguage) take from it what they keep.
 */
object RuleNotationLibrary : PrologLibrary("ruleNotation") {
    // The operators of the rules of a theory, for the theory of this module and the solvers using it
    override val libraryOperators: OperatorSet
        get() = RuleNotation.rules

    override val theoryOperators: OperatorSet
        get() = RuleNotation.theories

    override val notation: OperatorSet
        get() = RuleNotation.rules

    override val prologRawTheory: String = THEORY
}
