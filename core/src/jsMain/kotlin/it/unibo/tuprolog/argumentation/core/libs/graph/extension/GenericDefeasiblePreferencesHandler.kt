package it.unibo.tuprolog.argumentation.core.libs.graph.extension

actual object GenericDefeasiblePreferencesHandler : GenericDefeasiblePreferencesHandlerBase() {
    override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.GenericDefPreferences.theoryCode
}
