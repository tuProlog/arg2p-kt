package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

/**
 * The stable labellings (module `stable`, an argumentLabellingMode): those leaving no argument UND, built one
 * strongly connected component at a time (see the module scc).
 */
object StableLabeller : PrologModule("prolog.argumentation.graph.labelling.stable", "stable") {
    override val prologRawTheory: String =
        """
        %! locality(-Locality)
        %  Query mode: stable labellings depend on the whole graph (an odd cycle anywhere leaves none)
        locality(whole_graph).

        %! argumentLabelling
        %  Stable labellings are SCC-recursive: built component by component, leaving no argument UND
        argumentLabelling :-
            scc::label(decided, all).
        """.trimIndent()
}
