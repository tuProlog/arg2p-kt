package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

abstract class BpPartialLabellerBase : PrologModule("prolog.argumentation.graph.labelling.bpgroundedpartial", "bp_grounded_partial")

expect object BpPartialLabeller : BpPartialLabellerBase {
    override val prologRawTheory: String
}
