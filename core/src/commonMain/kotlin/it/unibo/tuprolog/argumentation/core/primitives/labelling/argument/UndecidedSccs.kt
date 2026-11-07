package it.unibo.tuprolog.argumentation.core.primitives.labelling.argument

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.intArg
import it.unibo.tuprolog.argumentation.core.engine.replyItems
import it.unibo.tuprolog.argumentation.core.primitives.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.primitives.graph.idList
import it.unibo.tuprolog.argumentation.core.primitives.graph.idsArg
import it.unibo.tuprolog.argumentation.core.primitives.graph.labelledIds
import it.unibo.tuprolog.argumentation.core.primitives.graph.stronglyConnectedComponents
import it.unibo.tuprolog.solve.Signature

/**
 * undecidedSccs(+Scc, +Branch, -Pieces): the strongly connected components of the UND arguments of Scc in Branch, by the
 * attacks among them alone, in topological order. Removing arguments only splits a component, so these are what is
 * left of Scc to search, and each of them is a component of what is still UND in the graph.
 */
internal object UndecidedSccs : AbstractNativePrimitive(Signature("undecidedSccs", 3)) {
    override fun call(call: NativeCall): Int {
        val scc = call.idsArg(0, signature)
        val undecided = call.labelledIds("und", call.intArg(1, signature).toInt())
        val left = scc.filter { it in undecided }.toIntArray()
        // (all of Scc still UND: Scc itself, strongly connected)
        if (left.size == scc.size) return call.replyItems(2, intArrayOf(call.arg(0)))
        if (left.size <= 1) return call.replyItems(2, IntArray(left.size) { call.idList(left) })
        val graph = call.attackGraph()
        val pieces = stronglyConnectedComponents(left) { graph.attacked(it) }
        return call.replyItems(2, IntArray(pieces.size) { call.idList(pieces[it]) })
    }
}
