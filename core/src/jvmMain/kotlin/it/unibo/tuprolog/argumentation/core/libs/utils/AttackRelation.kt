package it.unibo.tuprolog.argumentation.core.libs.utils

import it.unibo.tuprolog.argumentation.core.TheoryUtils

actual object AttackRelation : AttackRelationBase() {
    override val prologRawTheory: String
        get() = TheoryUtils.loadTheoryFromPrologFile("utils/attackRelation")
}
