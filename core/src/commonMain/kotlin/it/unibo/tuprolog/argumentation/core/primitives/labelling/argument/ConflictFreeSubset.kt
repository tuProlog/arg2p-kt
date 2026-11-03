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
 * conflictFreeSubset(+Args, +Forced, -Set): the subsets of Args including Forced where no argument attacks another
 * one (itself included), each once. The search only adds arguments compatible with those already chosen, so every
 * branch ends in a solution: the cost grows with the number of sets, not with the subsets of Args.
 */
internal object ConflictFreeSubset : AbstractNativePrimitive(Signature("conflictFreeSubset", 3)) {
    override fun call(call: NativeCall): Int {
        val args = call.termsArg(0, signature)
        val forced = call.termsArg(1, signature).toSet()
        val graph = call.conflictGraph()

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
        return call.answers(2, subsets(0, emptyList()).map { LogicList.of(it) }.iterator())
    }
}
