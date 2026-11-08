package it.unibo.tuprolog.argumentation.core.modules.labelling.argument

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The admissible labellings (module `admissible`, an argumentLabellingMode): the conflict-free ones in which
 * every attacker of an IN argument is attacked by an IN argument.
 */
object AdmissibleModule : PrologModule("prolog.argumentation.graph.labelling.admissible", "admissible") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! locality(-Locality)
        %  Query mode: the label of an argument only depends on the arguments it depends on (directionality)
        locality(directional).

        %! argumentLabelling
        %  Each admissible labelling in turn, left as the active context: the search of the SCC-recursive semantics,
        %  its labellings not closed (what the IN arguments defend need not be IN), every admissible one kept.
        argumentLabelling :-
            scc::label(admissible, all, none).
        """.trimIndent()
}
