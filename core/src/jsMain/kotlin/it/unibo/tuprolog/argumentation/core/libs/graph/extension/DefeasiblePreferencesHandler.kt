package it.unibo.tuprolog.argumentation.core.libs.graph.extension

actual object DefeasiblePreferencesHandler : DefeasiblePreferencesHandlerBase() {
    actual override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.DefPreferences.theoryCode
}
