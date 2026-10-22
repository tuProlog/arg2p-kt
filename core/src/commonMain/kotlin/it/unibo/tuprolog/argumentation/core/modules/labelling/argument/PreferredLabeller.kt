package it.unibo.tuprolog.argumentation.core.modules.labelling.argument

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

/**
 * The preferred labellings (module `preferred`, an argumentLabellingMode): the admissible ones with a maximal set of
 * IN arguments, built one strongly connected component at a time (see the module scc).
 */
object PreferredLabeller : PrologModule("prolog.argumentation.graph.labelling.preferred", "preferred") {
    override val prologRawTheory: String =
        """
        %! locality(-Locality)
        %  Query mode: the label of an argument only depends on the arguments it depends on (directionality)
        locality(directional).

        %! argumentLabelling
        %  Preferred labellings are SCC-recursive: built component by component, keeping the admissible local
        %  labellings with a maximal set of IN arguments
        argumentLabelling :-
            scc::label(admissible, maximal).
        """.trimIndent()
}
