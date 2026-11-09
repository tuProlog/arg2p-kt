package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.preference.standardPref

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.preference.standardPref.prolog.THEORY

/**
 * The preferences between the rules of the theory, applied to the attacks (module `standardPref`, a graphExtension):
 * an attack fails when the attacked argument is superior to the attacker (see the module superiority).
 */
object StandardPrefLibrary : PrologLibrary("standardPref") {
    override val prologRawTheory: String = THEORY
}
