package it.unibo.tuprolog.argumentation.core.primitives.labelling.argument

import it.unibo.tuprolog.argumentation.core.primitives.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.primitives.graph.idList
import it.unibo.tuprolog.argumentation.core.primitives.graph.idsArg
import it.unibo.tuprolog.argumentation.core.primitives.graph.labelledIds
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

        fun possible(d: Int): Boolean {
            if (d !in members || d !in undecided || d in excluded) return false
            for (target in graph.attacked(d)) if (target == d || target in inIds) return false
            for (attacker in graph.attackers(d)) if (attacker !in members && attacker in undecided) return false
            return true
        }

        var best: IntArray? = null
        for (u in scc) {
            if (u !in inIds) continue
            for (a in graph.attackers(u)) {
                if (a in outIds) continue
                val left = graph.attackers(a).filter { it != a && possible(it) }.distinct()
                if (best == null || left.size < best.size) {
                    best = left.toIntArray()
                    if (best.size <= 1) return best
                }
            }
        }
        return best
    }
}
