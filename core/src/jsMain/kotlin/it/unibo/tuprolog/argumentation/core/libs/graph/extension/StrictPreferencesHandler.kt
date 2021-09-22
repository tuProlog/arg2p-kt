package it.unibo.tuprolog.argumentation.core.libs.graph.extension

import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

actual object StrictPreferencesHandler: StrictPreferencesHandlerBase() {
    override val prologTheory: Theory
        get() = Theory.parse(it.unibo.tuprolog.argumentation.core.libs.sources.Preferences.theoryCode)
}
