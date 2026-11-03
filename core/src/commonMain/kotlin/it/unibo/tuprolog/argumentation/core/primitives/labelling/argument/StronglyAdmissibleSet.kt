package it.unibo.tuprolog.argumentation.core.primitives.labelling.argument

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.primitives.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.primitives.graph.labelledIds
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.Signature

/**
 * stronglyAdmissibleSet: every IN argument is reachable from the unattacked ones, iteratively adding the IN
 * arguments whose attackers are all attacked by those added so far
 */
internal object StronglyAdmissibleSet : AbstractNativePrimitive(Signature("stronglyAdmissibleSet", 0)) {
    override fun call(call: NativeCall): Int {
        val attackers = call.attackGraph().attackers
        val built = mutableSetOf<Term>()
        var pending = call.labelledIds("in")
        while (pending.isNotEmpty()) {
            val added =
                pending.filter { h ->
                    attackers[h].orEmpty().all { attacker -> attackers[attacker].orEmpty().any { it in built } }
                }
            if (added.isEmpty()) return NativeCall.FAIL
            built += added
            pending = pending - added.toSet()
        }
        return NativeCall.CONTINUE
    }
}
