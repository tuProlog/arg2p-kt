package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

abstract class BpLabellerBase : PrologModule("prolog.argumentation.graph.labelling.bpgrounded", "bp_grounded")

expect object BpLabeller : BpLabellerBase {
    override val prologRawTheory: String
}
