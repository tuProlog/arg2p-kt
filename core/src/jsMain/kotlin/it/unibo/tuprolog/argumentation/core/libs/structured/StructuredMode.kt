package it.unibo.tuprolog.argumentation.core.libs.structured

actual object StructuredMode : StructuredModeBase() {
    override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.QueryMode.theoryCode
}
