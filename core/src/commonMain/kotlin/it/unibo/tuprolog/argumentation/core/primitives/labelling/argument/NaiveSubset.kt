package it.unibo.tuprolog.argumentation.core.primitives.labelling.argument

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.answers
import it.unibo.tuprolog.argumentation.core.engine.termsArg
import it.unibo.tuprolog.argumentation.core.primitives.graph.conflictGraph
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.core.List as LogicList

/**
 * naiveSubset(+Args, +Forced, +In, -Set): the maximal conflict-free subsets of Args including Forced, each once:
 * every argument of Args left out either attacks itself or conflicts with an argument of Set or In (already IN).
 * An argument left out that no later argument can conflict with prunes the search.
 */
internal object NaiveSubset : AbstractNativePrimitive(Signature("naiveSubset", 4)) {
    override fun call(call: NativeCall): Int {
        val args = call.termsArg(0, signature)
        val forced = call.termsArg(1, signature).toSet()
        val alreadyIn = call.termsArg(2, signature)
        val graph = call.conflictGraph()

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
        return call.answers(3, subsets(0, emptyList(), emptyList()).map { LogicList.of(it) }.iterator())
    }
}
