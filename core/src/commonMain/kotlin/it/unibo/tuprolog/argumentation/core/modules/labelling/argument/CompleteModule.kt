package it.unibo.tuprolog.argumentation.core.modules.labelling.argument

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The complete labellings (module `complete`, an argumentLabellingMode): the admissible ones labelling IN every
 * argument they defend, built one strongly connected component at a time (see the module scc).
 */
object CompleteModule : PrologModule("prolog.argumentation.graph.labelling.complete", "complete") {
    override val prologRawTheory: String =
        """
        % Complete labellings are SCC-recursive: built component by component, keeping the admissible local ones

        %! locality(-Locality)
        %  Query mode: the label of an argument only depends on the arguments it depends on (directionality)
        locality(directional).

        %! argumentLabelling
        %  Each complete labelling in turn, left as the active context.
        argumentLabelling :-
            scc::label(admissible, all).
        """.trimIndent()
}
