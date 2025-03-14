package it.unibo.tuprolog.argumentation.core.libs.utils

actual object AttackRelation : AttackRelationBase() {
    override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.AttackRelation.theoryCode
}
