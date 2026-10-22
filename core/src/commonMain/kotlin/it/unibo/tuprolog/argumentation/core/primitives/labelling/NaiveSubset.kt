package it.unibo.tuprolog.argumentation.core.primitives.labelling

import it.unibo.tuprolog.argumentation.core.mining.graph
import it.unibo.tuprolog.argumentation.core.primitives.graph.conflictGraph
import it.unibo.tuprolog.argumentation.core.primitives.ids
import it.unibo.tuprolog.argumentation.core.primitives.replyList
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

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
