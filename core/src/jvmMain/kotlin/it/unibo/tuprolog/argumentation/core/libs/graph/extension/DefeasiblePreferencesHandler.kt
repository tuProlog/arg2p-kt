package it.unibo.tuprolog.argumentation.core.libs.graph.extension

import it.unibo.tuprolog.argumentation.core.TheoryUtils
import it.unibo.tuprolog.theory.Theory

actual object DefeasiblePreferencesHandler: DefeasiblePreferencesHandlerBase() {
    override val prologTheory: Theory
        get() = TheoryUtils.loadTheoryFromPrologFile("core/graph/extensions/defPreferences")
}
