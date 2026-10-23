package it.unibo.tuprolog.argumentation.core.modules.labelling.argument

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The stage2 labellings (module `stage2`, an argumentLabellingMode): as cf2, with the stage labellings in each
 * strongly connected component.
 */
object Stage2Module : PrologModule("prolog.argumentation.graph.labelling.stage2", "stage2") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! locality(-Locality)
        %  Query mode: the label of an argument only depends on the arguments it depends on (directionality)
        locality(directional).

        %! argumentLabelling
        %  Each stage2 labelling in turn, left as the active context.
        argumentLabelling :-
            cf2::compute(stage).
        
        """.trimIndent()
}
