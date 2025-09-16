package it.unibo.tuprolog.argumentation.core.libs.graph.extension

actual object AttackRestrictionHandler : AttackRestrictionHandlerBase() {
    actual override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.AttackRestriction.theoryCode
}
