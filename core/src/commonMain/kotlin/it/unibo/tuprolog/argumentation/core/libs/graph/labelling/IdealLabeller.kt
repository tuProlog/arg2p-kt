package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

/**
 * The ideal labelling (module `ideal`, an argumentLabellingMode): the maximal admissible subset of the arguments IN
 * in every preferred labelling.
 */
object IdealLabeller : PrologModule("prolog.argumentation.graph.labelling.ideal", "ideal") {
    override val prologRawTheory: String =
        """
        %! locality(-Locality)
        %  Query mode: the label of an argument only depends on the arguments it depends on (directionality)
        locality(directional).

        %! argumentLabelling
        %  The ideal labelling has IN the maximal admissible subset of the intersection of the preferred extensions
        argumentLabelling :-
            context_active(Branch),
            findall(In, (
                preferred:::argumentLabelling,
                findall(A, context_check(inId(A)), In)
            ), Preferred),
            utils::intersection(Preferred, Common),
            admissible::maximal_admissible_subset(Common, Ideal),
            conflictfree::labelSet(Branch, Ideal).
        """.trimIndent()
}
