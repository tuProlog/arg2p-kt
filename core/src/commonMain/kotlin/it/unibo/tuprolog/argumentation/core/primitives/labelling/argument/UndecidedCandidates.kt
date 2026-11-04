package it.unibo.tuprolog.argumentation.core.primitives.labelling.argument

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.intArg
import it.unibo.tuprolog.argumentation.core.engine.replyWord
import it.unibo.tuprolog.argumentation.core.primitives.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.primitives.graph.idList
import it.unibo.tuprolog.argumentation.core.primitives.graph.idsArg
import it.unibo.tuprolog.argumentation.core.primitives.graph.labelledIds
import it.unibo.tuprolog.argumentation.core.runtime.context.IdSet
import it.unibo.tuprolog.solve.Signature

/**
 * undecidedCandidates(+Scc, +Branch, -Ids): the UND arguments of Scc in Branch that can be forced IN, those not
 * attacked by an UND argument of a previous component (nothing left could defend them)
 */
internal object UndecidedCandidates : AbstractNativePrimitive(Signature("undecidedCandidates", 3)) {
    override fun call(call: NativeCall): Int {
        val scc = call.idsArg(0, signature)
        val members = IdSet.of(scc)
        val undecided = call.labelledIds("und", call.intArg(1, signature).toInt())
        val graph = call.attackGraph()
        val candidates =
            scc.filter { id ->
                id in undecided && graph.attackers(id).none { it !in members && it in undecided }
            }
        return call.replyWord(2, call.idList(candidates.toIntArray()))
    }
}
