package it.unibo.tuprolog.argumentation.core.modules.labelling.argument

import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.primitives.labelling.ConflictFreeSubset
import it.unibo.tuprolog.argumentation.core.primitives.labelling.LabelSet
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The conflict-free labellings (module `conflictfree`, an argumentLabellingMode): a set of arguments IN, none
 * attacking another one, those they attack OUT and the rest UND. The other semantics take from it the arguments to
 * label and the labelling of a set.
 */
object ConflictFreeModule : PrologModule("prolog.argumentation.graph.labelling.conflictfree", "conflictfree") {
    override val primitives: Map<Signature, Primitive>
        get() = mapOf(ConflictFreeSubset.signature to ConflictFreeSubset, LabelSet.signature to LabelSet)

    override val prologRawTheory: String =
        """
        %! locality(-Locality)
        %  Query mode: the label of an argument only depends on the arguments it depends on (directionality)
        locality(directional).

        %! argumentLabelling
        %  Each conflict-free labelling in turn, left as the active context.
        argumentLabelling :-
            context_active(Branch),
            candidates(Args, Forced),
            conflict_free_subset(Args, Forced, Set),
            labelSet(Branch, Set).

        %! candidates(-Args, -Forced)
        %  The arguments to label, and those among them already IN (kept IN)
        candidates(Args, Forced) :-
            graphStore::argumentIds(Args),
            findall(A, (member(A, Args), graphStore::argumentLabelById(in, A)), Forced).

        %! labelSet(+Branch, +Set)
        %  In a new branch of Branch, Set IN, the arguments it attacks OUT, the rest UND;
        %  the branch is released on backtracking
        labelSet(Branch, Set) :-
            label_set(Branch, Set),
            context_active(Labelling),
            (
                true
            ;
                context_checkout(Branch),
                context_drop(Labelling),
                fail
            ).
        """.trimIndent()
}
