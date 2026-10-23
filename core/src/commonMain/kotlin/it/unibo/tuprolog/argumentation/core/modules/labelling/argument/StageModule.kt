package it.unibo.tuprolog.argumentation.core.modules.labelling.argument

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The stage labellings (module `stage`, an argumentLabellingMode): the naive ones with a minimal set of UND
 * arguments.
 */
object StageModule : PrologModule("prolog.argumentation.graph.labelling.stage", "stage") {
    override val prologRawTheory: String =
        // language=Prolog
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
                findall(A, graphStore::argumentLabelById(und, A), Und),
                findall(A, graphStore::argumentLabelById(in, A), In)
            ), Naive),
            utils::minimalBySet(Naive, Stage),
            member(In, Stage),
            conflictfree::labelSet(Branch, In).
        """.trimIndent()
}
