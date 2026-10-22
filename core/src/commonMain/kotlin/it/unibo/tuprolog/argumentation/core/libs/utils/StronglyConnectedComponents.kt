package it.unibo.tuprolog.argumentation.core.libs.utils

import it.unibo.tuprolog.argumentation.core.libs.graph.argumentIds
import it.unibo.tuprolog.argumentation.core.libs.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.libs.graph.ids
import it.unibo.tuprolog.argumentation.core.libs.graph.replyList
import it.unibo.tuprolog.argumentation.core.libs.graph.stronglyConnectedComponents
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

// Primitives on the attack graph, for the module utils

/**
 * sccs(-Sccs): strongly connected components of the attack graph, as lists of argument identifiers,
 * in topological order (a component is only attacked by itself and by the components before it).
 * sccs(+Ids, -Sccs): the same, for the subgraph induced by the arguments in Ids.
 */
class StronglyConnectedComponents(
    arity: Int,
) : Primitive {
    val signature: Signature = Signature("sccs", arity)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val nodes = if (request.arguments.size == 1) request.argumentIds() else request.ids(0)
        val attacked = request.attackGraph().attacked
        val sccs = stronglyConnectedComponents(nodes) { attacked[it].orEmpty() }
        return sequenceOf(request.replyList(request.arguments.lastIndex, sccs.map { List.of(it) }) ?: request.replyFail())
    }
}
