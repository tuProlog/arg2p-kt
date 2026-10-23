package it.unibo.tuprolog.argumentation.core.modules.labelling.argument

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The eager labelling (module `eager`, an argumentLabellingMode): the maximal admissible subset of the arguments IN
 * in every semi-stable labelling.
 */
object EagerModule : PrologModule("prolog.argumentation.graph.labelling.eager", "eager") {
    override val prologRawTheory: String =
        """
        %! locality(-Locality)
        %  Query mode: not directional, but the labellings of unconnected parts of the graph are independent: the label
        %  of an argument depends on the arguments connected to it, in either direction
        locality(connected).

        %! argumentLabelling
        %  The eager labelling has IN the maximal admissible subset of the intersection of the semi-stable extensions
        argumentLabelling :-
            context_active(Branch),
            findall(In, (
                semistable:::argumentLabelling,
                findall(A, graphStore::argumentLabelById(in, A), In)
            ), Semistable),
            utils::intersection(Semistable, Common),
            admissible::maximal_admissible_subset(Common, Eager),
            conflictfree::labelSet(Branch, Eager).
        """.trimIndent()
}
