package it.unibo.tuprolog.argumentation.core.modules.labelling.argument

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The semi-stable labellings (module `semistable`, an argumentLabellingMode): the preferred ones with a minimal set
 * of UND arguments.
 */
object SemistableModule : PrologModule("prolog.argumentation.graph.labelling.semistable", "semistable") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! locality(-Locality)
        %  Query mode: not directional, but the labellings of unconnected parts of the graph are independent: the label
        %  of an argument depends on the arguments connected to it, in either direction
        locality(connected).

        %! argumentLabelling
        %  Semi-stable labellings are the preferred ones with a minimal set of UND arguments
        %  (an admissible set with a maximal range is a maximal admissible set)
        argumentLabelling :-
            context_active(Branch),
            findall(Und-In, (
                preferred:::argumentLabelling,
                findall(A, graphStore::argumentLabelById(und, A), Und),
                findall(A, graphStore::argumentLabelById(in, A), In)
            ), Preferred),
            utils::minimal_by_set(Preferred, Semistable),
            member(In, Semistable),
            conflictfree::labelSet(Branch, In).
        """.trimIndent()
}
