package it.unibo.tuprolog.argumentation.core.modules.labelling.argument

import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.primitives.labelling.GroundedFixpoint
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The grounded labelling (module `grounded`, an argumentLabellingMode, the default one): the least fixpoint of IN
 * the arguments whose attackers are all OUT and OUT those attacked by an IN one, computed natively
 * (grounded_fixpoint).
 */
object GroundedModule : PrologModule("prolog.argumentation.graph.labelling.grounded", "grounded") {
    override val primitives: Map<Signature, Primitive>
        get() = mapOf(GroundedFixpoint.signature to GroundedFixpoint)

    override val prologRawTheory: String =
        """
        %! locality(-Locality)
        %  Query mode: the label of an argument only depends on the arguments it depends on (directionality)
        locality(directional).

        %! argumentLabelling
        %  The grounded labelling, in the active context.
        argumentLabelling :-
            grounded_fixpoint.
        """.trimIndent()
}
