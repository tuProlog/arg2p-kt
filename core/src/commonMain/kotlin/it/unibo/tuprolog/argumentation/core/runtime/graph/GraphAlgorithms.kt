package it.unibo.tuprolog.argumentation.core.runtime.graph

/** The items sorted, each once (the array itself is sorted and overwritten) */
internal fun IntArray.sortedOnce(): IntArray {
    sort()
    var n = 0
    for (i in indices) if (i == 0 || this[i] != this[i - 1]) this[n++] = this[i]
    return copyOf(n)
}

/**
 * Strongly connected components of a directed graph (Tarjan's algorithm, iterative), in topological order:
 * every edge between two components goes from an earlier one to a later one. Nodes keep their input order
 * within a component, and successors outside nodes are ignored.
 */
internal fun stronglyConnectedComponents(
    nodes: IntArray,
    successors: (Int) -> IntArray,
): List<IntArray> {
    val places = IntArray((nodes.maxOrNull() ?: -1) + 1) { -1 }
    for (i in nodes.indices) places[nodes[i]] = i
    val index = IntArray(nodes.size) { -1 }
    val lowLink = IntArray(nodes.size)
    val onStack = BooleanArray(nodes.size)
    val stack = IntArray(nodes.size)
    var stackTop = 0
    // the explicit call stack: a place, its successors, the next of them to look at
    val callPlaces = IntArray(nodes.size)
    val callSuccessors = arrayOfNulls<IntArray>(nodes.size)
    val callNext = IntArray(nodes.size)
    var callTop = 0
    var visited = 0
    val components = ArrayList<IntArray>()

    fun visit(place: Int) {
        index[place] = visited
        lowLink[place] = visited
        visited++
        stack[stackTop++] = place
        onStack[place] = true
        callPlaces[callTop] = place
        callSuccessors[callTop] = successors(nodes[place])
        callNext[callTop] = 0
        callTop++
    }
    for (root in nodes.indices) {
        if (index[root] != -1 || places[nodes[root]] != root) continue
        visit(root)
        while (callTop > 0) {
            val top = callTop - 1
            val place = callPlaces[top]
            val next = callSuccessors[top]!!
            if (callNext[top] < next.size) {
                val id = next[callNext[top]++]
                val successor = if (id < places.size) places[id] else -1
                if (successor == -1) continue
                if (index[successor] == -1) {
                    visit(successor)
                } else if (onStack[successor]) {
                    lowLink[place] = minOf(lowLink[place], index[successor])
                }
                continue
            }
            callTop--
            callSuccessors[top] = null
            if (callTop > 0) {
                val caller = callPlaces[callTop - 1]
                lowLink[caller] = minOf(lowLink[caller], lowLink[place])
            }
            if (lowLink[place] == index[place]) {
                var first = stackTop
                do {
                    first--
                    onStack[stack[first]] = false
                } while (stack[first] != place)
                val members = stack.copyOfRange(first, stackTop)
                members.sort()
                components += IntArray(members.size) { nodes[members[it]] }
                stackTop = first
            }
        }
    }
    // Tarjan completes a component after all the components reachable from it
    return components.asReversed()
}
