package it.unibo.tuprolog.argumentation.core.modules.labelling.argument

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The ideal labelling (module `ideal`, an argumentLabellingMode): the maximal admissible subset of the arguments IN
 * in every preferred labelling.
 */
object IdealModule : PrologModule("prolog.argumentation.graph.labelling.ideal", "ideal") {
    override val prologRawTheory: String =
        // language=Prolog
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
                findall(A, graphStore::argumentLabelById(in, A), In)
            ), Preferred),
            utils::intersection(Preferred, Common),
            admissible::maximalAdmissibleSubset(Common, Ideal),
            conflictfree::labelSet(Branch, Ideal).
        """.trimIndent()
}
