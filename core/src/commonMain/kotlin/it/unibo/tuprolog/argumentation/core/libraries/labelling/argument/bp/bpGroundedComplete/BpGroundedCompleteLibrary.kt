package it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.bp.bpGroundedComplete

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.bp.bpGroundedComplete.prolog.THEORY

/**
 * The complete labelling with burdens of persuasion (module `bpGroundedComplete`, an argumentLabellingMode, ICLP):
 * the partial labelling, then the grounded labelling of its UND arguments, repeated until nothing changes.
 */
object BpGroundedCompleteLibrary : PrologLibrary("bpGroundedComplete") {
    override val prologRawTheory: String = THEORY
}
