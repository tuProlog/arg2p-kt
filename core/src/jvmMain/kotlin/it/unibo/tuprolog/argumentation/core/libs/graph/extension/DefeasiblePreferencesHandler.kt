package it.unibo.tuprolog.argumentation.core.libs.graph.extension

import it.unibo.tuprolog.argumentation.core.TheoryUtils

actual object DefeasiblePreferencesHandler : DefeasiblePreferencesHandlerBase() {
    actual override val prologRawTheory: String
        get() = TheoryUtils.loadTheoryFromPrologFile("core/graph/extensions/defPreferences")
}
