package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.preference.defeasiblePref

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.preference.defeasiblePref.prolog.THEORY

/**
 * The preferences between rules as conclusions of the theory (module `defeasiblePref`, a graphExtension): an
 * argument for `sup(Rule, Other)` makes the attacks that preference invalidates fail, as long as it stands.
 *
 * Each attack it may invalidate becomes an argument for `attack(Type, Attacker, Attacked, On)` (top rule `attack`),
 * supported by the attacker and attacking in its place; the arguments for the preferences invalidating it attack it
 * (attacks of type `pref`).
 */
object DefeasiblePrefLibrary : PrologLibrary("defeasiblePref") {
    override val prologRawTheory: String = THEORY
}
