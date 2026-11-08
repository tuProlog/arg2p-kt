package it.unibo.tuprolog.argumentation.core.primitives.labelling.argument

import it.unibo.tuprolog.argumentation.core.runtime.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.runtime.graph.labelledIds
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.words.IdSet
import it.unibo.tuprolog.solve.Signature

/**
 * stronglyAdmissibleSet: every IN argument is reachable from the unattacked ones, iteratively adding the IN
 * arguments whose attackers are all attacked by those added so far
 */
internal object StronglyAdmissibleSet : AbstractNativePrimitive(Signature("stronglyAdmissibleSet", 0)) {
    override fun call(call: NativeCall): Int {
        val graph = call.attackGraph()
        val built = IdSet()
        var pending = call.labelledIds("in").toIntArray()
        while (pending.isNotEmpty()) {
            val added =
                pending.filter { h ->
                    graph.attackers(h).all { attacker -> graph.attackers(attacker).any { it in built } }
                }
            if (added.isEmpty()) return NativeCall.FAIL
            added.forEach { built.add(it) }
            pending = pending.filter { it !in built }.toIntArray()
        }
        return NativeCall.CONTINUE
    }
}
