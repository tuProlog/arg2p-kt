package it.unibo.tuprolog.argumentation.core.modules.labelling.argument

import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.primitives.labelling.AdmissibleSet
import it.unibo.tuprolog.argumentation.core.primitives.labelling.MaximalAdmissibleSubset
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The admissible labellings (module `admissible`, an argumentLabellingMode): the conflict-free ones in which
 * every attacker of an IN argument is attacked by an IN argument.
 */
object AdmissibleModule : PrologModule("prolog.argumentation.graph.labelling.admissible", "admissible") {
    override val primitives: Map<Signature, Primitive>
        get() =
            (listOf(AdmissibleSet(0), AdmissibleSet(1)).associateBy { it.signature }) +
                (MaximalAdmissibleSubset.signature to MaximalAdmissibleSubset)

    override val prologRawTheory: String =
        """
        %! locality(-Locality)
        %  Query mode: the label of an argument only depends on the arguments it depends on (directionality)
        locality(directional).

        %! argumentLabelling
        %  Each admissible labelling in turn, left as the active context.
        argumentLabelling :-
            context_active(Branch),
            conflictfree::candidates(Args, Forced),
            conflictfree::conflict_free_subset(Args, Forced, Set),
            admissible_set(Set),
            conflictfree::labelSet(Branch, Set).
        """.trimIndent()
}
