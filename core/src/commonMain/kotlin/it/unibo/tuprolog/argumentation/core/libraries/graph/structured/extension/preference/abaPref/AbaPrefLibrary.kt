package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.preference.abaPref

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.preference.abaPref.prolog.THEORY

/**
 * The preferences between the assumptions of ABA+ (module `abaPref`, a graphExtension, of the profile aba_plus;
 * Čyras and Toni 2016): `sup(Stronger, Weaker)`, closed transitively. An attack on an assumption by an argument
 * resting on an assumption weaker than it is reversed: it is removed, and the argument for the stronger assumption
 * attacks every argument whose support (its assumptions, see the module abaLanguage) contains the whole support of
 * the attacker. The reversal hits the arguments resting on the same assumptions as the attacker, not only the
 * attacker: as ABA+ reverses attacks between sets of assumptions, and the arguments do not share a sub-argument for
 * those sets. That holds on the theories satisfying Weak Contraposition (Čyras and Toni 2016, Axiom 4), and those
 * failing it are refused: their semantics are those of sets of assumptions, which the arguments do not capture.
 */
object AbaPrefLibrary : PrologLibrary("abaPref") {
    override val prologRawTheory: String = THEORY
}
