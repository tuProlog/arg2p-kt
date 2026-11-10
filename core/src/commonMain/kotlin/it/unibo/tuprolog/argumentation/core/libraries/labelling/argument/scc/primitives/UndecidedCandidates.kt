package it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.scc.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.graph.idList
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.graph.idsArg
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.graph.labelledIds
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.intArg
import it.unibo.tuprolog.argumentation.engine.vm.replyWord
import it.unibo.tuprolog.argumentation.engine.words.IdSet
import it.unibo.tuprolog.solve.Signature

/**
 * undecidedCandidates(+Scc, +Branch, +Excluded, -Ids): the UND arguments of Scc in Branch, not in Excluded, the search
 * forces IN next. While an attacker of an IN argument of Scc is attacked by no IN argument, every admissible labelling above
 * Branch has one of its possible defenders IN (see forcing): the candidates are those of the attacker with the fewest.
 * Otherwise they are the UND arguments of Scc not attacked by an UND argument of a previous component (nothing left could
 * defend them).
 */
internal object UndecidedCandidates : AbstractNativePrimitive(Signature("undecidedCandidates", 4)) {
    override fun call(call: NativeCall): Int {
        val scc = call.idsArg(0, signature)
        val members = IdSet.of(scc)
        val branch = call.intArg(1, signature).toInt()
        val undecided = call.labelledIds("und", branch)
        val excluded = IdSet.of(call.idsArg(2, signature))
        val graph = call.attackGraph()
        val defenders = defendersToChoose(call, scc, members, undecided, excluded, branch)
        val candidates =
            defenders ?: scc
                .filter { id ->
                    id in undecided && id !in excluded && graph.attackers(id).none { it !in members && it in undecided }
                }.toIntArray()
        return call.replyWord(3, call.idList(candidates))
    }

    // the fewest possible defenders of an attacker of an IN argument of Scc that no IN argument attacks; null when there is none
    private fun defendersToChoose(
        call: NativeCall,
        scc: IntArray,
        members: IdSet,
        undecided: IdSet,
        excluded: IdSet,
        branch: Int,
    ): IntArray? {
        val inIds = call.labelledIds("in", branch)
        val outIds = call.labelledIds("out", branch)
        val graph = call.attackGraph()

        var best: IntArray? = null
        for (u in scc) {
            if (u !in inIds) continue
            for (a in graph.attackers(u)) {
                if (a in outIds) continue
                val left = graph.attackers(a).filter { it != a && graph.canBeForced(it, members, undecided, excluded, inIds) }.distinct()
                if (best == null || left.size < best.size) {
                    best = left.toIntArray()
                    if (best.size <= 1) return best
                }
            }
        }
        return best
    }
}
