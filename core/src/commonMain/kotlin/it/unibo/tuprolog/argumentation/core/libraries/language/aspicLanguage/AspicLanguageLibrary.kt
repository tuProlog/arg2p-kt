package it.unibo.tuprolog.argumentation.core.libraries.language.aspicLanguage

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.aspicLanguage.prolog.THEORY

/**
 * The language of ASPIC+ (module `aspicLanguage`, a graphLanguage of the profiles aspic_plus and aspic): the one of
 * arg2p, well formed (Modgil and Prakken 2013). A contrary, a conflict declared one way only (conflict([X], [Y]) with
 * no conflict([Y], [X])), targets defeasible statements alone: neither an axiom nor the conclusion of a strict rule,
 * which no attack could reach. The contradictories (negations, conflicts declared both ways) are those transposition
 * closes the strict rules over.
 */
object AspicLanguageLibrary : PrologLibrary("aspicLanguage") {
    override val prologRawTheory: String = THEORY
}
