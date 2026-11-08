package it.unibo.tuprolog.argumentation.core.primitives.labelling.argument

import it.unibo.tuprolog.argumentation.core.primitives.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.primitives.graph.graphArguments
import it.unibo.tuprolog.argumentation.core.primitives.graph.labelArguments
import it.unibo.tuprolog.argumentation.core.primitives.graph.labelledIds
import it.unibo.tuprolog.argumentation.core.primitives.graph.unlabelArguments
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.intArg
import it.unibo.tuprolog.solve.Signature

/**
 * labelIn(+Id): the UND argument Id labelled IN and the UND arguments it attacks OUT, and nothing else: the step of the
 * search of the SCC-recursive semantics when the labellings need not be closed (admissible sets), where propagateFrom
 * would also label IN what the IN arguments defend. Fails if Id is not UND.
 */
internal object LabelIn : AbstractNativePrimitive(Signature("labelIn", 1)) {
    override fun call(call: NativeCall): Int {
        val arguments = call.graphArguments()
        val graph = call.attackGraph()
        val id = call.intArg(0, signature).toInt()
        val start = arguments.position(id)
        val undecided = call.labelledIds("und")
        if (start == -1 || id !in undecided) return NativeCall.FAIL
        val targets = ArrayList<Int>()
        for (target in graph.attacked(id)) {
            val at = arguments.position(target)
            if (at != -1 && at != start && target in undecided) targets += at
        }
        val out = targets.toIntArray()
        out.sort()
        var n = 0
        for (i in out.indices) if (i == 0 || out[i] != out[i - 1]) out[n++] = out[i]
        val newOut = out.copyOf(n)
        call.labelArguments(arguments, "in", intArrayOf(start))
        if (newOut.isNotEmpty()) call.labelArguments(arguments, "out", newOut)
        call.unlabelArguments(arguments, "und", intArrayOf(start) + newOut)
        return NativeCall.CONTINUE
    }
}
