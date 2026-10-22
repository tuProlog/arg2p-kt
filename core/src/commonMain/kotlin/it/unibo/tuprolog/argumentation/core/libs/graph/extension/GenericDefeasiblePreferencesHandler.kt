package it.unibo.tuprolog.argumentation.core.libs.graph.extension

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

abstract class GenericDefeasiblePreferencesHandlerBase :
    PrologModule("prolog.argumentation.graph.preferences.defeasible.generic", "defeasibleAllPref")

expect object GenericDefeasiblePreferencesHandler : GenericDefeasiblePreferencesHandlerBase {
    override val prologRawTheory: String
}
