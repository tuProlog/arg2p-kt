package it.unibo.tuprolog.argumentation.core.primitives.labelling.argument

import it.unibo.tuprolog.argumentation.core.primitives.graph.conflictGraph
import it.unibo.tuprolog.argumentation.core.primitives.ids
import it.unibo.tuprolog.argumentation.core.primitives.replyList
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * conflictFreeSubset(+Args, +Forced, -Set): the subsets of Args including Forced where no argument attacks another
 * one (itself included), each once. The search only adds arguments compatible with those already chosen, so every
 * branch ends in a solution: the cost grows with the number of sets, not with the subsets of Args.
 */
object ConflictFreeSubset : Primitive {
    val signature: Signature = Signature("conflictFreeSubset", 3)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val args = request.ids(0)
        val forced = request.ids(1).toSet()
        val graph = request.conflictGraph()

        fun subsets(
            i: Int,
            chosen: List<Term>,
        ): Sequence<List<Term>> =
            sequence {
                if (i == args.size) {
                    yield(chosen)
                    return@sequence
                }
                val a = args[i]
                if (!graph.selfAttacking(a) && chosen.none { graph.conflict(a, it) }) yieldAll(subsets(i + 1, chosen + a))
                if (a !in forced) yieldAll(subsets(i + 1, chosen))
            }
        return subsets(0, emptyList()).mapNotNull { request.replyList(2, it) }
    }
}
