package it.unibo.tuprolog.argumentation.core.libs.graph.extension

actual object StrictPreferencesHandler : StrictPreferencesHandlerBase() {
    actual override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.Preferences.theoryCode
}
