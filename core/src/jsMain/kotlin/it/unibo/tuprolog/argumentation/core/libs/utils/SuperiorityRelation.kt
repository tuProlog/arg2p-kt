package it.unibo.tuprolog.argumentation.core.libs.utils

actual object SuperiorityRelation : SuperiorityRelationBase() {
    override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.SuperiorityRelation.theoryCode
}
