package it.unibo.tuprolog.argumentation.core.libs.graph.extension

import it.unibo.tuprolog.argumentation.core.TheoryUtils

actual object StrictPreferencesHandler : StrictPreferencesHandlerBase() {
    override val prologRawTheory: String
        get() = TheoryUtils.loadTheoryFromPrologFile("core/graph/extensions/preferences")
}
