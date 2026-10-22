package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

abstract class GroundedLabellerBase : PrologModule("prolog.argumentation.graph.labelling.grounded.old", "grounded_old")

expect object GroundedLabeller : GroundedLabellerBase {
    override val prologRawTheory: String
}
