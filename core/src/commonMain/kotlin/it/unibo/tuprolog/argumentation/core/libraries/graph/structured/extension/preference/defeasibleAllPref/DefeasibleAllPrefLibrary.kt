package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.preference.defeasibleAllPref

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.preference.defeasibleAllPref.prolog.THEORY

/**
 * The preferences between rules as conclusions of the theory, taken together (module `defeasibleAllPref`, a
 * graphExtension): as defeasiblePref, but the attack argument is attacked by an argument for all the preferences
 * invalidating it, `preference(Preferences)`, supported by one argument for each of them: one such argument for each
 * choice of those arguments, each standing or falling with its own.
 */
object DefeasibleAllPrefLibrary : PrologLibrary("defeasibleAllPref") {
    override val prologRawTheory: String = THEORY
}
