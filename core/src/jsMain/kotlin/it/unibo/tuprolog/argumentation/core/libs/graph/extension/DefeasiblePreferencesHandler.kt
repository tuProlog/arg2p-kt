package it.unibo.tuprolog.argumentation.core.libs.graph.extension

import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

actual object DefeasiblePreferencesHandler: DefeasiblePreferencesHandlerBase() {
    override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.DefPreferences.theoryCode
}
