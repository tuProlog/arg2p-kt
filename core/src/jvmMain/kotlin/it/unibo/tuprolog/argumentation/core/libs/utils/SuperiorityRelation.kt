package it.unibo.tuprolog.argumentation.core.libs.utils

import it.unibo.tuprolog.argumentation.core.TheoryUtils

actual object SuperiorityRelation : SuperiorityRelationBase() {
    override val prologRawTheory: String
        get() = TheoryUtils.loadTheoryFromPrologFile("utils/superiorityRelation")
}
