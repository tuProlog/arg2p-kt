package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

/**
 * The stage labellings (module `stage`, an argumentLabellingMode): the naive ones with a minimal set of UND
 * arguments.
 */
object StageLabeller : PrologModule("prolog.argumentation.graph.labelling.stage", "stage") {
    override val prologRawTheory: String =
        """
        %! locality(-Locality)
        %  Query mode: not directional, but the labellings of unconnected parts of the graph are independent: the label
        %  of an argument depends on the arguments connected to it, in either direction
        locality(connected).

        %! argumentLabelling
        %  Stage labellings are the naive ones with a minimal set of UND arguments
        argumentLabelling :-
            context_active(Branch),
            findall(Und-In, (
                naive:::argumentLabelling,
                findall(A, context_check(undId(A)), Und),
                findall(A, context_check(inId(A)), In)
            ), Naive),
            utils::minimal_by_set(Naive, Stage),
            member(In, Stage),
            conflictfree::labelSet(Branch, In).
        """.trimIndent()
}
