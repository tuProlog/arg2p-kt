package it.unibo.tuprolog.argumentation.core.libraries.language.abaLanguage

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.abaLanguage.prolog.THEORY

/**
 * The language of assumption-based argumentation (module `abaLanguage`, a graphLanguage of the profile aba), mapped
 * to the one of arg2p as flat ABA is an instance of ASPIC+ (Prakken 2010): each assumption (assumption(A)) an ordinary
 * premise asm(A), each contrary (contrary(A, C)) the conflict of C with A, one way (an argument for C attacks those
 * resting on A). Rules are the strict rules of arg2p.
 */
object AbaLanguageLibrary : PrologLibrary("abaLanguage") {
    override val prologRawTheory: String = THEORY
}
