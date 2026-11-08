package it.unibo.tuprolog.argumentation.core.primitives.utils

import it.unibo.tuprolog.argumentation.core.primitives.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.primitives.graph.graphArguments
import it.unibo.tuprolog.argumentation.core.primitives.graph.idList
import it.unibo.tuprolog.argumentation.core.primitives.graph.idsArg
import it.unibo.tuprolog.argumentation.core.primitives.graph.stronglyConnectedComponents
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.replyItems
import it.unibo.tuprolog.solve.Signature

/**
 * sccs(-Sccs): strongly connected components of the attack graph, as lists of argument identifiers,
 * in topological order (a component is only attacked by itself and by the components before it).
 * sccs(+Ids, -Sccs): strongly connected components of the subgraph of the arguments in Ids, as lists of argument
 * identifiers, in topological order.
 */
internal class Sccs(
    arity: Int,
) : AbstractNativePrimitive(Signature("sccs", arity)) {
    override fun call(call: NativeCall): Int {
        val nodes = if (call.arity == 1) call.graphArguments().ids else call.idsArg(0, signature)
        val graph = call.attackGraph()
        val sccs = stronglyConnectedComponents(nodes) { graph.attacked(it) }
        return call.replyItems(call.arity - 1, IntArray(sccs.size) { call.idList(sccs[it]) })
    }
}
