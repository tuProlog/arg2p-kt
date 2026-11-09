package it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.bp.bpGroundedPartial

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.bp.bpGroundedPartial.prolog.THEORY

/**
 * The partial labelling with burdens of persuasion (module `bp_grounded_partial`, an argumentLabellingMode, ICLP):
 * each argument labelled IN*, OUT* or UND* by the conditions of the burdens on its conclusion and its complement.
 */
object BpGroundedPartialLibrary : PrologLibrary("bp_grounded_partial") {
    override val prologRawTheory: String = THEORY
}
