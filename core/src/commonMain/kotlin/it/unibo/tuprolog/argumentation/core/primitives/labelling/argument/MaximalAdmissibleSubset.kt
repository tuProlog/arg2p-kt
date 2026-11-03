package it.unibo.tuprolog.argumentation.core.primitives.labelling.argument

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.replyTerms
import it.unibo.tuprolog.argumentation.core.engine.termsArg
import it.unibo.tuprolog.argumentation.core.primitives.graph.attackGraph
import it.unibo.tuprolog.solve.Signature

/**
 * maximalAdmissibleSubset(+Ids, -Subset): the largest admissible subset of the conflict-free set Ids, removing the
 * arguments with an attacker not attacked by the remaining ones until none is left (a greatest fixpoint, polynomial)
 */
internal object MaximalAdmissibleSubset : AbstractNativePrimitive(Signature("maximalAdmissibleSubset", 2)) {
    override fun call(call: NativeCall): Int {
        val attackers = call.attackGraph().attackers
        var subset = call.termsArg(0, signature)
        while (true) {
            val defended =
                subset.filter { h ->
                    attackers[h].orEmpty().all { attacker ->
                        attackers[attacker].orEmpty().any { defendant -> defendant in subset && defendant != attacker }
                    }
                }
            if (defended.size == subset.size) break
            subset = defended
        }
        return call.replyTerms(1, subset)
    }
}
