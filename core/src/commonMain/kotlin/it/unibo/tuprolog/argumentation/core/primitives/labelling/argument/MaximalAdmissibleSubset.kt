package it.unibo.tuprolog.argumentation.core.primitives.labelling.argument

import it.unibo.tuprolog.argumentation.core.primitives.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.primitives.graph.idList
import it.unibo.tuprolog.argumentation.core.primitives.graph.idsArg
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.replyWord
import it.unibo.tuprolog.argumentation.engine.words.IdSet
import it.unibo.tuprolog.solve.Signature

/**
 * maximalAdmissibleSubset(+Ids, -Subset): the largest admissible subset of the conflict-free set Ids, removing the
 * arguments with an attacker not attacked by the remaining ones until none is left (a greatest fixpoint, polynomial)
 */
internal object MaximalAdmissibleSubset : AbstractNativePrimitive(Signature("maximalAdmissibleSubset", 2)) {
    override fun call(call: NativeCall): Int {
        val graph = call.attackGraph()
        var subset = call.idsArg(0, signature)
        while (true) {
            val members = IdSet.of(subset)
            val defended = subset.filter { graph.defended(it, members) }.toIntArray()
            if (defended.size == subset.size) break
            subset = defended
        }
        return call.replyWord(1, call.idList(subset))
    }
}
