package it.unibo.tuprolog.argumentation.core.libraries.language.defeasibleLanguage

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.defeasibleLanguage.prolog.THEORY
import it.unibo.tuprolog.argumentation.core.libraries.language.ruleNotation.notation.RuleNotation
import it.unibo.tuprolog.core.operators.OperatorSet

/**
 * The defeasible part of the language of ASPIC+ (module `defeasibleLanguage`, a graphLanguage of the profile
 * aspicDefeasible): defeasible rules alone, facts as rules with no premise (`r : [] => a.`). It adds no premise nor
 * conflict; it checks the theory has no strict rule, axiom nor premise.
 */
object DefeasibleLanguageLibrary : PrologLibrary("defeasibleLanguage") {
    // The notation its theories are written with: defeasible rules alone (r : a => b, facts as r : [] => a): premises and axioms do not read
    override val notation: OperatorSet
        get() = RuleNotation.label + RuleNotation.defeasible

    override val prologRawTheory: String = THEORY
}
