package it.unibo.tuprolog.argumentation.core.primitives.labelling.argument

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.reply
import it.unibo.tuprolog.argumentation.core.primitives.graph.AttackGraph
import it.unibo.tuprolog.argumentation.core.primitives.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.primitives.graph.idsArg
import it.unibo.tuprolog.argumentation.core.primitives.graph.labelledIds
import it.unibo.tuprolog.argumentation.core.runtime.context.IdSet
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
 * admissibleSet: every attacker of an IN argument is attacked by an IN argument other than itself.
 * admissibleSet(+Ids): every attacker of an IN argument, or of an argument in Ids, is attacked by one of them other
 * than itself (the arguments in Ids taken as IN).
 */
internal class AdmissibleSet(
    arity: Int,
) : AbstractNativePrimitive(Signature("admissibleSet", arity)) {
    override fun call(call: NativeCall): Int {
        val labelled = call.labelledIds("in")
        val inIds = if (call.arity == 0) labelled else labelled.copy().addAll(call.idsArg(0, signature))
        val graph = call.attackGraph()
        for (i in 0 until inIds.size) if (!graph.defended(inIds[i], inIds)) return call.reply(false)
        return call.reply(true)
    }
}
