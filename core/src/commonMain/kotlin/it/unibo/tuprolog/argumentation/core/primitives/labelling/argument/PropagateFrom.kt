package it.unibo.tuprolog.argumentation.core.primitives.labelling.argument

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.intArg
import it.unibo.tuprolog.argumentation.core.primitives.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.primitives.graph.graphArguments
import it.unibo.tuprolog.argumentation.core.primitives.graph.labelArguments
import it.unibo.tuprolog.argumentation.core.primitives.graph.labelledIds
import it.unibo.tuprolog.argumentation.core.primitives.graph.unlabelArguments
import it.unibo.tuprolog.argumentation.core.runtime.context.IdSet
import it.unibo.tuprolog.solve.Signature

/**
 * propagateFrom(+Id): in a context labelled with a grounded fixpoint (every argument IN, OUT or UND, as
 * groundedFixpoint leaves it), the UND argument Id labelled IN and the fixpoint extended from it: what the full
 * groundedFixpoint would compute after unlabelling the UND arguments and labelling Id IN, by the cascade of Id alone.
 *
 * The labels are asserted in the rounds and the order of groundedFixpoint (IN Id, then OUT the targets of the arguments
 * just labelled IN, then IN those whose attackers are now all OUT, and so on), and only the arguments whose label
 * changes lose their UND label: the store ends as the full computation leaves it, at a cost of the arguments the cascade
 * reaches.
 */
internal object PropagateFrom : AbstractNativePrimitive(Signature("propagateFrom", 1)) {
    override fun call(call: NativeCall): Int {
        val arguments = call.graphArguments()
        val graph = call.attackGraph()
        val start = arguments.position(call.intArg(0, signature).toInt())
        val undecided = call.labelledIds("und")
        if (start == -1 || arguments.ids[start] !in undecided) return NativeCall.FAIL
        val outIds = call.labelledIds("out").copy()
        val changed = IdSet()

        fun und(place: Int) = arguments.ids[place] in undecided && arguments.ids[place] !in changed

        // (sorted and each once, as groundedFixpoint labels a round)
        fun round(places: List<Int>): IntArray {
            val sorted = places.toIntArray()
            sorted.sort()
            var n = 0
            for (i in sorted.indices) if (i == 0 || sorted[i] != sorted[i - 1]) sorted[n++] = sorted[i]
            return sorted.copyOf(n)
        }

        val labelled = ArrayList<IntArray>()
        var newIn = intArrayOf(start)
        changed.add(arguments.ids[start])
        call.labelArguments(arguments, "in", newIn)
        labelled += newIn
        while (true) {
            val targets = ArrayList<Int>()
            for (place in newIn) {
                for (target in graph.attacked(arguments.ids[place])) {
                    val at = arguments.position(target)
                    if (at != -1 && und(at)) targets += at
                }
            }
            val newOut = round(targets)
            if (newOut.isEmpty()) break
            call.labelArguments(arguments, "out", newOut)
            for (place in newOut) {
                outIds.add(arguments.ids[place])
                changed.add(arguments.ids[place])
            }
            labelled += newOut
            val released = ArrayList<Int>()
            for (place in newOut) {
                for (target in graph.attacked(arguments.ids[place])) {
                    val at = arguments.position(target)
                    if (at != -1 && und(at) && graph.attackers(target).all { it in outIds }) released += at
                }
            }
            newIn = round(released)
            if (newIn.isEmpty()) break
            call.labelArguments(arguments, "in", newIn)
            for (place in newIn) changed.add(arguments.ids[place])
            labelled += newIn
        }
        val gone = IntArray(labelled.sumOf { it.size })
        var n = 0
        for (places in labelled) for (place in places) gone[n++] = place
        call.unlabelArguments(arguments, "und", gone)
        return NativeCall.CONTINUE
    }
}
