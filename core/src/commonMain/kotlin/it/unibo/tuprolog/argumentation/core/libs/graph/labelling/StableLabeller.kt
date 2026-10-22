package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

object StableLabeller : PrologModule("prolog.argumentation.graph.labelling.stable", "stable") {
    override val prologRawTheory: String =
        """
        % Query mode: stable labellings depend on the whole graph (an odd cycle anywhere leaves none)
        locality(whole_graph).

        % Stable labellings are SCC-recursive: built component by component, leaving no argument UND
        argumentLabelling :-
            scc::label(decided, all).
        """.trimIndent()
}
