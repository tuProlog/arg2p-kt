package it.unibo.tuprolog.argumentation.core.primitives.labelling.argument

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.reply
import it.unibo.tuprolog.argumentation.core.engine.termsArg
import it.unibo.tuprolog.argumentation.core.primitives.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.primitives.graph.labelledIds
import it.unibo.tuprolog.solve.Signature

/**
 * admissibleSet: every attacker of an IN argument is attacked by an IN argument other than itself.
 * admissibleSet(+Ids): every attacker of an IN argument, or of an argument in Ids, is attacked by one of them other
 * than itself (the arguments in Ids taken as IN).
 */
internal class AdmissibleSet(
    arity: Int,
) : AbstractNativePrimitive(Signature("admissibleSet", arity)) {
    override fun call(call: NativeCall): Int {
        val inIds = call.labelledIds("in") + if (call.arity == 0) emptyList() else call.termsArg(0, signature)
        val attackers = call.attackGraph().attackers
        val admissible =
            inIds.all { h ->
                attackers[h].orEmpty().all { attacker ->
                    attackers[attacker].orEmpty().any { defendant -> defendant in inIds && defendant != attacker }
                }
            }
        return call.reply(admissible)
    }
}
