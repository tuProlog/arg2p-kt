package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

object CompleteLabeller : PrologModule("prolog.argumentation.graph.labelling.complete", "complete") {
    override val prologRawTheory: String =
        """
        % Complete labellings are SCC-recursive: built component by component, keeping the admissible local ones

        % Query mode: the label of an argument only depends on the arguments it depends on (directionality)
        locality(directional).

        argumentLabelling :-
            scc::label(admissible, all).
        """.trimIndent()
}
