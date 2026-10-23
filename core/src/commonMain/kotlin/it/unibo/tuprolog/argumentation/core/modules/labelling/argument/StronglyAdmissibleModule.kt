package it.unibo.tuprolog.argumentation.core.modules.labelling.argument

import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.primitives.labelling.StronglyAdmissibleSet
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The strongly admissible labellings (module `stronglyadmissible`, an argumentLabellingMode): the admissible ones
 * whose IN arguments are defended, step by step, starting from the unattacked ones.
 */
object StronglyAdmissibleModule : PrologModule("prolog.argumentation.graph.labelling.stronglyadmissible", "stronglyadmissible") {
    override val primitives: Map<Signature, Primitive>
        get() = mapOf(StronglyAdmissibleSet.signature to StronglyAdmissibleSet)

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! locality(-Locality)
        %  Query mode: the label of an argument only depends on the arguments it depends on (directionality)
        locality(directional).

        %! argumentLabelling
        %  Each strongly admissible labelling in turn, left as the active context.
        argumentLabelling :-
            admissible::argumentLabelling,
            strongly_admissible_set.
        """.trimIndent()
}
