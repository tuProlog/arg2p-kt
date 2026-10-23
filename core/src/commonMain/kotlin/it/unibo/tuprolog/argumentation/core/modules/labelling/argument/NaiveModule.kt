package it.unibo.tuprolog.argumentation.core.modules.labelling.argument

import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.primitives.labelling.NaiveSubset
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The naive labellings (module `naive`, an argumentLabellingMode): the conflict-free ones with a maximal set of IN
 * arguments.
 */
object NaiveModule : PrologModule("prolog.argumentation.graph.labelling.naive", "naive") {
    override val primitives: Map<Signature, Primitive>
        get() = mapOf(NaiveSubset.signature to NaiveSubset)

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! locality(-Locality)
        %  Query mode: not directional, but the labellings of unconnected parts of the graph are independent: the label
        %  of an argument depends on the arguments connected to it, in either direction
        locality(connected).

        %! argumentLabelling
        %  Each naive labelling in turn, left as the active context.
        argumentLabelling :-
            context_active(Branch),
            conflictfree::candidates(Args, Forced),
            findall(A, graphStore::argumentLabelById(in, A), In),
            naive_subset(Args, Forced, In, Set),
            conflictfree::labelSet(Branch, Set).
        """.trimIndent()
}
