package it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.labelSets.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.graph.idList
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.graph.idsArg
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.natives.replyWord
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
