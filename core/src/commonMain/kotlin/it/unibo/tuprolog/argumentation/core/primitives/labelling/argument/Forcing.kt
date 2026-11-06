package it.unibo.tuprolog.argumentation.core.primitives.labelling.argument

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.replyWord
import it.unibo.tuprolog.argumentation.core.primitives.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.primitives.graph.idsArg
import it.unibo.tuprolog.argumentation.core.primitives.graph.labelledIds
import it.unibo.tuprolog.argumentation.core.runtime.context.IdSet
import it.unibo.tuprolog.solve.Signature

/**
 * forcing(+Scc, +Excluded, -Next): in a state of the search of the component Scc (a grounded fixpoint), what the defence of
 * its IN arguments forces, the arguments of Excluded kept out of IN. An attacker of an IN argument of Scc that no IN
 * argument attacks needs a defender, and only an UND argument of Scc not excluded can still become one: not attacking an IN
 * argument, not attacking itself, and attacked by no UND argument of an earlier component (whose labels are final, see
 * undecidedCandidates). With none, no admissible labelling without the excluded extends the state, and forcing fails, as
 * it does when an excluded argument is IN; with one, every such labelling has it IN, and Next is `defender(Id)`; otherwise
 * Next is `none`.
 */
internal object Forcing : AbstractNativePrimitive(Signature("forcing", 3)) {
    override fun call(call: NativeCall): Int {
        val scc = call.idsArg(0, signature)
        val members = IdSet.of(scc)
        val excluded = call.idsArg(1, signature)
        val inIds = call.labelledIds("in")
        for (e in excluded) if (e in inIds) return NativeCall.FAIL
        val excludedIds = IdSet.of(excluded)
        val outIds = call.labelledIds("out")
        val undIds = call.labelledIds("und")
        val graph = call.attackGraph()

        // whether an UND argument of Scc can still be forced IN
        fun possible(d: Int): Boolean {
            if (d !in members || d !in undIds || d in excludedIds) return false
            for (target in graph.attacked(d)) if (target == d || target in inIds) return false
            for (attacker in graph.attackers(d)) if (attacker !in members && attacker in undIds) return false
            return true
        }

        var forced = -1
        for (u in scc) {
            if (u !in inIds) continue
            for (a in graph.attackers(u)) {
                if (a in outIds) continue
                var defender = -1
                var count = 0
                for (d in graph.attackers(a)) {
                    if (d == a || !possible(d)) continue
                    if (count == 0 || d != defender) count++
                    defender = d
                    if (count > 1) break
                }
                if (count == 0) return NativeCall.FAIL
                if (count == 1 && forced == -1) forced = defender
            }
        }
        val next = if (forced == -1) call.atom("none") else call.struct("defender", intArrayOf(call.int(forced)))
        return call.replyWord(2, next)
    }
}
