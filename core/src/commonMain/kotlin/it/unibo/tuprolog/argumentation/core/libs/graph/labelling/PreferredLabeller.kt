package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

object PreferredLabeller : PrologModule("prolog.argumentation.graph.labelling.preferred", "preferred") {
    override val prologRawTheory: String =
        """
        % Query mode: the label of an argument only depends on the arguments it depends on (directionality)
        locality(directional).

        % Preferred labellings are SCC-recursive: built component by component, keeping the admissible local
        % labellings with a maximal set of IN arguments
        argumentLabelling :-
            scc::label(admissible, maximal).
        """.trimIndent()
}
