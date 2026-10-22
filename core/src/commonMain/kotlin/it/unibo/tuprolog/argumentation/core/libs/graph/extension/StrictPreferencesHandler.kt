package it.unibo.tuprolog.argumentation.core.libs.graph.extension

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

abstract class StrictPreferencesHandlerBase : PrologModule("prolog.argumentation.graph.preferences.strict", "standardPref")

expect object StrictPreferencesHandler : StrictPreferencesHandlerBase {
    override val prologRawTheory: String
}
