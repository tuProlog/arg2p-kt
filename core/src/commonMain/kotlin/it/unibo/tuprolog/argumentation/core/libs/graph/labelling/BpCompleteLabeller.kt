package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

abstract class BpCompleteLabellerBase : PrologModule("prolog.argumentation.graph.labelling.bpgroundedcomplete", "bp_grounded_complete")

expect object BpCompleteLabeller : BpCompleteLabellerBase {
    override val prologRawTheory: String
}
