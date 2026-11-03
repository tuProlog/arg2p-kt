package it.unibo.tuprolog.argumentation.core.primitives.labelling.argument

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.replyTerms
import it.unibo.tuprolog.argumentation.core.engine.termArg
import it.unibo.tuprolog.argumentation.core.engine.termsArg
import it.unibo.tuprolog.argumentation.core.primitives.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.primitives.graph.labelledIds
import it.unibo.tuprolog.solve.Signature

/**
 * undecidedCandidates(+Scc, +Branch, -Ids): the UND arguments of Scc in Branch that can be forced IN, those not
 * attacked by an UND argument of a previous component (nothing left could defend them)
 */
internal object UndecidedCandidates : AbstractNativePrimitive(Signature("undecidedCandidates", 3)) {
    override fun call(call: NativeCall): Int {
        val scc = call.termsArg(0, signature)
        val undecided = call.labelledIds("und", call.termArg(1))
        val attackers = call.attackGraph().attackers
        val candidates =
            scc.filter { id ->
                id in undecided && attackers[id].orEmpty().none { it !in scc && it in undecided }
            }
        return call.replyTerms(2, candidates)
    }
}
