package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

object Stage2Labeller : PrologModule("prolog.argumentation.graph.labelling.stage2", "stage2") {
    override val prologRawTheory: String =
        """
        % Query mode: the label of an argument only depends on the arguments it depends on (directionality)
        locality(directional).

        argumentLabelling :-
            cf2::compute(stage).
        
        """.trimIndent()
}
