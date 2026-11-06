package it.unibo.tuprolog.argumentation.core.modules.labelling.argument

import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.primitives.labelling.argument.GroundedFixpoint
import it.unibo.tuprolog.argumentation.core.primitives.labelling.argument.PropagateFrom
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The grounded labelling (module `grounded`, an argumentLabellingMode, the default one): the least fixpoint of IN
 * the arguments whose attackers are all OUT and OUT those attacked by an IN one, computed natively
 * (groundedFixpoint).
 */
object GroundedModule : PrologModule("prolog.argumentation.graph.labelling.grounded", "grounded") {
    override val primitives: Map<Signature, Primitive>
        get() = listOf(GroundedFixpoint, PropagateFrom).associateBy { it.signature }

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! locality(-Locality)
        %  Query mode: the label of an argument only depends on the arguments it depends on (directionality)
        locality(directional).

        %! argumentLabelling
        %  The grounded labelling, in the active context.
        argumentLabelling :-
            groundedFixpoint.

        %! propagateFrom(+Id)
        %  In a context labelled with a grounded fixpoint, the UND argument Id labelled IN and the fixpoint extended
        %  from it alone: the labels argumentLabelling would give after unlabelling UND and labelling Id IN.
        """.trimIndent()
}
