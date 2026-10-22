package it.unibo.tuprolog.argumentation.core.libs.graph.extension

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

abstract class DefeasiblePreferencesHandlerBase : PrologModule("prolog.argumentation.graph.preferences.defeasible", "defeasiblePref")

expect object DefeasiblePreferencesHandler : DefeasiblePreferencesHandlerBase {
    override val prologRawTheory: String
}
