package it.unibo.tuprolog.argumentation.core.libraries.language.abstractLanguage

import it.unibo.tuprolog.argumentation.core.libraries.language.abstractLanguage.prolog.THEORY

/**
 * The language of Dung frameworks (module `abstractLanguage`, the graphLanguage of the profile abstract): arguments
 * and attacks alone, which the module abstractGraph reads. It adds no premise nor conflict; it checks the theory has
 * none of the notations of the structured profiles, which an abstract framework would drop.
 */
internal object AbstractFrameworkLanguageLibrary : AbstractNotationLibrary("abstractLanguage") {
    override val prologRawTheory: String = THEORY
}
