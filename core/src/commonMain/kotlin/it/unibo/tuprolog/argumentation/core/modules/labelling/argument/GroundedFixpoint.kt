package it.unibo.tuprolog.argumentation.core.modules.labelling.argument

import it.unibo.tuprolog.argumentation.core.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.graph.graphArguments
import it.unibo.tuprolog.argumentation.core.graph.labelArguments
import it.unibo.tuprolog.argumentation.core.graph.labelledIds
import it.unibo.tuprolog.argumentation.core.graph.sortedOnce
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.words.IdSet
import it.unibo.tuprolog.solve.Signature

/**
 * groundedFixpoint: extends the IN/OUT labelling of the active context to the grounded fixpoint, labelling UND
 * the arguments left. Each round labels IN the arguments whose attackers are all OUT, then OUT the arguments
 * attacked by an IN one; the context is read once and only the new labels are asserted.
 *
 * Each argument keeps the number of its attackers not OUT: a round only looks at the arguments the previous one
 * changed (those attacked by the arguments just labelled), rather than at all the unlabelled ones, which made a
 * chain of attacks quadratic. The labels of a round are asserted in the order of the arguments.
 */
internal object GroundedFixpoint : AbstractNativePrimitive(Signature("groundedFixpoint", 0)) {
    override fun call(call: NativeCall): Int {
        val arguments = call.graphArguments()
        val ids = arguments.ids
        val graph = call.attackGraph()
        val inIds = call.labelledIds("in").copy()
        val outIds = call.labelledIds("out").copy()

        // the arguments here are by their places in the graph
        fun unlabelled(place: Int) = ids[place] !in inIds && ids[place] !in outIds

        // the attackers not OUT of each argument unlabelled at the start, -1 for the others
        val pending =
            IntArray(ids.size) { place ->
                if (unlabelled(place)) graph.attackers(ids[place]).count { it !in outIds } else -1
            }

        // The places, those unlabelled, labelled with [name] in order, each once
        fun label(
            places: IntArray,
            name: String,
        ): IntArray {
            val sorted = places.sortedOnce()
            if (sorted.isNotEmpty()) call.labelArguments(arguments, name, sorted)
            return sorted
        }

        // the arguments to label IN, and those to label OUT, in the next round
        var candidatesIn = ids.indices.filter { pending[it] == 0 }.toIntArray()
        var candidatesOut =
            ids.indices.filter { place -> unlabelled(place) && graph.attackers(ids[place]).any { it in inIds } }.toIntArray()
        do {
            val newIn = label(candidatesIn.filter { unlabelled(it) && pending[it] == 0 }.toIntArray(), "in")
            for (place in newIn) inIds.add(ids[place])
            val attackedByIn = ArrayList<Int>()
            for (place in newIn) {
                for (target in graph.attacked(ids[place])) {
                    val at = arguments.position(target)
                    if (at != -1) attackedByIn += at
                }
            }
            val newOut = label((candidatesOut.asList() + attackedByIn).filter { unlabelled(it) }.toIntArray(), "out")
            for (place in newOut) outIds.add(ids[place])
            val released = IdSet()
            for (place in newOut) {
                for (target in graph.attacked(ids[place])) {
                    val at = arguments.position(target)
                    if (at == -1 || pending[at] == -1) continue
                    pending[at]--
                    if (pending[at] == 0) released.add(at)
                }
            }
            candidatesIn = released.toIntArray()
            candidatesOut = IntArray(0)
        } while (newIn.isNotEmpty() || newOut.isNotEmpty())
        call.labelArguments(arguments, "und", ids.indices.filter { unlabelled(it) }.toIntArray())
        return NativeCall.CONTINUE
    }
}
