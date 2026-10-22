package it.unibo.tuprolog.argumentation.core.libs.utils

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

abstract class AttackRelationBase : PrologModule("prolog.argumentation.attack", "attack")

expect object AttackRelation : AttackRelationBase {
    override val prologRawTheory: String
}
