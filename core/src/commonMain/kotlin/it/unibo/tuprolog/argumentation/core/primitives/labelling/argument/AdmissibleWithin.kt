package it.unibo.tuprolog.argumentation.core.primitives.labelling.argument

import it.unibo.tuprolog.argumentation.core.runtime.graph.AttackGraph
import it.unibo.tuprolog.argumentation.core.runtime.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.runtime.graph.idsArg
import it.unibo.tuprolog.argumentation.core.runtime.graph.labelledIds
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.reply
import it.unibo.tuprolog.argumentation.engine.words.IdSet
import it.unibo.tuprolog.solve.Signature

/** Whether every attacker of the arguments [ids] is attacked by one of them other than itself */
internal fun AttackGraph.defended(
    id: Int,
    ids: IdSet,
): Boolean {
    for (attacker in attackers(id)) {
        var answered = false
        for (defendant in attackers(attacker)) {
            if (defendant != attacker && defendant in ids) {
                answered = true
                break
            }
        }
        if (!answered) return false
    }
    return true
}

/**
 * admissibleWithin(+Scc): every attacker of an IN argument of the component Scc is attacked by an IN argument other
 * than itself. In a search one component at a time that holds of every IN argument: the IN arguments of earlier components
 * were checked when their component was, their attackers are in their own or earlier components, labelled for good, and
 * forcing arguments IN later only adds defenders.
 */
internal object AdmissibleWithin : AbstractNativePrimitive(Signature("admissibleWithin", 1)) {
    override fun call(call: NativeCall): Int {
        val inIds = call.labelledIds("in")
        val graph = call.attackGraph()
        for (id in call.idsArg(0, signature)) if (id in inIds && !graph.defended(id, inIds)) return call.reply(false)
        return call.reply(true)
    }
}
