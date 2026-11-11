package it.unibo.tuprolog.argumentation.core.libraries.framework.store.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.graph.graphArguments
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.graph.idList
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.graph.idsArg
import it.unibo.tuprolog.argumentation.core.libraries.framework.utils.helpers.stronglyConnectedComponents
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.natives.replyItems
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
