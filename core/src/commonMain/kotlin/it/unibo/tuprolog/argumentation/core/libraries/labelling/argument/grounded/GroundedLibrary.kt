package it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.grounded

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.grounded.primitives.GroundedFixpoint
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.grounded.primitives.PropagateFrom
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.grounded.prolog.THEORY
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The grounded labelling (module `grounded`, an argumentLabellingMode, the default one): the least fixpoint of IN
 * the arguments whose attackers are all OUT and OUT those attacked by an IN one, computed natively
 * (groundedFixpoint).
 */
object GroundedLibrary : PrologLibrary("grounded") {
    override val primitives: Map<Signature, Primitive>
        get() = listOf(GroundedFixpoint, PropagateFrom).associateBy { it.signature }

    override val prologRawTheory: String = THEORY
}
