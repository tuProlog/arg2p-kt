package it.unibo.tuprolog.argumentation.core.primitives.utils

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.replyTerms
import it.unibo.tuprolog.argumentation.core.engine.termsArg
import it.unibo.tuprolog.argumentation.core.primitives.graph.argumentIds
import it.unibo.tuprolog.argumentation.core.primitives.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.primitives.graph.stronglyConnectedComponents
import it.unibo.tuprolog.core.List
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
        val nodes = if (call.arity == 1) call.argumentIds() else call.termsArg(0, signature)
        val attacked = call.attackGraph().attacked
        val sccs = stronglyConnectedComponents(nodes) { attacked[it].orEmpty() }
        return call.replyTerms(call.arity - 1, sccs.map { List.of(it) })
    }
}
