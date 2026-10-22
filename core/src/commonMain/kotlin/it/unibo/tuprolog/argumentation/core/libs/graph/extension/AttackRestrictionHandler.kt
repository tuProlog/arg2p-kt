package it.unibo.tuprolog.argumentation.core.libs.graph.extension

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

abstract class AttackRestrictionHandlerBase : PrologModule("prolog.argumentation.graph.rebutrestriction", "rebutRestriction")

expect object AttackRestrictionHandler : AttackRestrictionHandlerBase {
    override val prologRawTheory: String
}
