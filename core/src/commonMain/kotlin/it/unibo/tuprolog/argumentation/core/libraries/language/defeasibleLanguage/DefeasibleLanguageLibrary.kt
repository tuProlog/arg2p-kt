package it.unibo.tuprolog.argumentation.core.libraries.language.defeasibleLanguage

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.defeasibleLanguage.prolog.THEORY

/**
 * The defeasible part of the language of ASPIC+ (module `defeasibleLanguage`, a graphLanguage of the profile
 * aspic_defeasible): defeasible rules alone, facts as rules with no premise (`r : [] => a.`). It adds no premise nor
 * conflict; it checks the theory has no strict rule, axiom nor premise.
 */
object DefeasibleLanguageLibrary : PrologLibrary("defeasibleLanguage") {
    override val prologRawTheory: String = THEORY
}
