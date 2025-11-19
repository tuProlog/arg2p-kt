package it.unibo.tuprolog.argumentation.core.libs.structured

import it.unibo.tuprolog.argumentation.core.TheoryUtils

actual object StructuredMode : StructuredModeBase() {
    actual override val prologRawTheory: String
        get() = TheoryUtils.loadTheoryFromPrologFile("core/queryMode")
}
