package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.PrologModule
import it.unibo.tuprolog.argumentation.core.libs.graph.store.conflictGraph
import it.unibo.tuprolog.argumentation.core.libs.ids
import it.unibo.tuprolog.argumentation.core.libs.replyList
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * The naive labellings (module `naive`, an argumentLabellingMode): the conflict-free ones with a maximal set of IN
 * arguments.
 */
object NaiveLabeller : PrologModule("prolog.argumentation.graph.labelling.naive", "naive") {
    override val primitives: Map<Signature, Primitive>
        get() = mapOf(NaiveSubset.signature to NaiveSubset)

    override val prologRawTheory: String =
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

/**
 * naive_subset(+Args, +Forced, +In, -Set): the maximal conflict-free subsets of Args including Forced, each once:
 * every argument of Args left out either attacks itself or conflicts with an argument of Set or In (already IN).
 * An argument left out that no later argument can conflict with prunes the search.
 */
object NaiveSubset : Primitive {
    val signature: Signature = Signature("naive_subset", 4)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val args = request.ids(0)
        val forced = request.ids(1).toSet()
        val alreadyIn = request.ids(2)
        val graph = request.conflictGraph()

        // pending: arguments left out that nothing chosen conflicts with yet
        fun subsets(
            i: Int,
            chosen: List<Term>,
            pending: List<Term>,
        ): Sequence<List<Term>> =
            sequence {
                if (pending.any { x -> (i until args.size).none { graph.conflict(x, args[it]) } }) return@sequence
                if (i == args.size) {
                    yield(chosen)
                    return@sequence
                }
                val a = args[i]
                if (!graph.selfAttacking(a) && chosen.none { graph.conflict(a, it) }) {
                    yieldAll(subsets(i + 1, chosen + a, pending.filterNot { graph.conflict(it, a) }))
                }
                if (a !in forced) {
                    val blocked = graph.selfAttacking(a) || (chosen + alreadyIn).any { graph.conflict(a, it) }
                    yieldAll(subsets(i + 1, chosen, if (blocked) pending else pending + a))
                }
            }
        return subsets(0, emptyList(), emptyList()).mapNotNull { request.replyList(3, it) }
    }
}
