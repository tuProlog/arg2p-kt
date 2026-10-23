package it.unibo.tuprolog.argumentation.core.primitives.graph

/**
 * Strongly connected components of a directed graph (Tarjan's algorithm, iterative), in topological order:
 * every edge between two components goes from an earlier one to a later one. Nodes keep their input order
 * within a component, and successors outside nodes are ignored.
 */
fun <T> stronglyConnectedComponents(
    nodes: Iterable<T>,
    successors: (T) -> Iterable<T>,
): List<List<T>> {
    val order = nodes.withIndex().associate { (i, node) -> node to i }
    val index = HashMap<T, Int>()
    val lowLink = HashMap<T, Int>()
    val onStack = HashSet<T>()
    val stack = ArrayDeque<T>()
    val components = mutableListOf<List<T>>()
    for (root in order.keys) {
        if (root in index) continue
        // explicit call stack of (node, iterator over its successors)
        val calls = ArrayDeque<Pair<T, Iterator<T>>>()

        fun visit(node: T) {
            index[node] = index.size
            lowLink[node] = index.getValue(node)
            stack.addLast(node)
            onStack += node
            calls.addLast(node to successors(node).filter { it in order }.iterator())
        }
        visit(root)
        while (calls.isNotEmpty()) {
            val (node, next) = calls.last()
            if (next.hasNext()) {
                val successor = next.next()
                if (successor !in index) {
                    visit(successor)
                } else if (successor in onStack) {
                    lowLink[node] = minOf(lowLink.getValue(node), index.getValue(successor))
                }
                continue
            }
            calls.removeLast()
            calls.lastOrNull()?.let { (caller, _) -> lowLink[caller] = minOf(lowLink.getValue(caller), lowLink.getValue(node)) }
            if (lowLink[node] == index[node]) {
                val component = mutableListOf<T>()
                do {
                    val member = stack.removeLast()
                    onStack -= member
                    component += member
                } while (member != node)
                components += component.sortedBy { order.getValue(it) }
            }
        }
    }
    // Tarjan completes a component after all the components reachable from it
    return components.asReversed()
}
