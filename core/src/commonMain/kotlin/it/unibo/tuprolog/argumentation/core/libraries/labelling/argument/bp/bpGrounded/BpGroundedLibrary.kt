package it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.bp.bpGrounded

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.bp.bpGrounded.language.primitives.TheoryBurdens
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.bp.bpGrounded.prolog.THEORY
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The grounded labelling with burdens of persuasion (module `bp_grounded`, an argumentLabellingMode, ICAIL): as the
 * grounded one, but an argument for a statement under a burden of persuasion that is not IN is OUT rather than UND.
 * Part of the semantics, it changes the graph it labels: a rebut or undermine by an argument under a burden of
 * persuasion fails unless the argument is superior to the one attacked (see filterBpDefeats/0).
 */
object BpGroundedLibrary : PrologLibrary("bp_grounded") {
    override val primitives: Map<Signature, Primitive>
        get() = mapOf(TheoryBurdens.signature to TheoryBurdens)

    override val prologRawTheory: String = THEORY
}
