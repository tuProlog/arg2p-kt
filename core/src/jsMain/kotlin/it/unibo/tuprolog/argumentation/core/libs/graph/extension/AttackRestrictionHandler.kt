package it.unibo.tuprolog.argumentation.core.libs.graph.extension

actual object AttackRestrictionHandler : AttackRestrictionHandlerBase() {
    override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.AttackRestriction.theoryCode
}
