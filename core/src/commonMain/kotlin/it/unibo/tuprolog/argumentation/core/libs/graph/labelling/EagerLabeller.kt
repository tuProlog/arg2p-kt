package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

object EagerLabeller : PrologModule("prolog.argumentation.graph.labelling.eager", "eager") {
    override val prologRawTheory: String =
        """
        % Query mode: not directional, but the labellings of unconnected parts of the graph are independent: the label
        % of an argument depends on the arguments connected to it, in either direction
        locality(connected).

        % The eager labelling has IN the maximal admissible subset of the intersection of the semi-stable extensions
        argumentLabelling :-
            context_active(Branch),
            findall(In, (
                semistable:::argumentLabelling,
                findall(A, context_check(inId(A)), In)
            ), Semistable),
            utils::intersection(Semistable, Common),
            admissible::maximal_admissible_subset(Common, Eager),
            conflictfree::labelSet(Branch, Eager).
        """.trimIndent()
}
