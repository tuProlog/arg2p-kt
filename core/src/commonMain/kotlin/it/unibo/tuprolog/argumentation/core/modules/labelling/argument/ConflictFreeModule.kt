package it.unibo.tuprolog.argumentation.core.modules.labelling.argument

import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The conflict-free labellings (module `conflictfree`, an argumentLabellingMode): a set of arguments IN, none
 * attacking another one, those they attack OUT and the rest UND.
 */
object ConflictFreeModule : PrologModule("prolog.argumentation.graph.labelling.conflictfree", "conflictfree") {
    override val primitives: Map<Signature, Primitive>
        get() = mapOf(ConflictFreeSubset.signature to ConflictFreeSubset)

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! locality(-Locality)
        %  Query mode: the label of an argument only depends on the arguments it depends on (directionality)
        locality(directional).

        %! argumentLabelling
        %  Each conflict-free labelling in turn, left as the active context.
        argumentLabelling :-
            context_active(Branch),
            labelling::candidates(Args, Forced),
            conflictFreeSubset(Args, Forced, Set),
            labelling::labelSet(Branch, Set).
        """.trimIndent()
}
