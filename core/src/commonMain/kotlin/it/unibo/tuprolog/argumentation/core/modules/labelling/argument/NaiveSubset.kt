package it.unibo.tuprolog.argumentation.core.modules.labelling.argument

import it.unibo.tuprolog.argumentation.core.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.graph.idsArg
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.answers
import it.unibo.tuprolog.argumentation.engine.words.IdSet
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.core.List as LogicList

/**
 * naiveSubset(+Args, +Forced, +In, -Set): the maximal conflict-free subsets of Args including Forced, each once:
 * every argument of Args left out either attacks itself or conflicts with an argument of Set or In (already IN).
 * An argument left out that no later argument can conflict with prunes the search.
 */
internal object NaiveSubset : AbstractNativePrimitive(Signature("naiveSubset", 4)) {
    override fun call(call: NativeCall): Int {
        val args = call.idsArg(0, signature)
        val forced = IdSet.of(call.idsArg(1, signature))
        val alreadyIn = call.idsArg(2, signature).asList()
        val graph = call.attackGraph()

        // pending: arguments left out that nothing chosen conflicts with yet
        fun subsets(
            i: Int,
            chosen: List<Int>,
            pending: List<Int>,
        ): Sequence<List<Int>> =
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
        return call.answers(3, subsets(0, emptyList(), emptyList()).map { set -> LogicList.of(set.map { Integer.of(it) }) }.iterator())
    }
}
