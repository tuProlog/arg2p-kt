package it.unibo.tuprolog.argumentation.core.primitives.graph

import it.unibo.tuprolog.argumentation.InternalArg2pApi
import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.replyWord
import it.unibo.tuprolog.argumentation.core.engine.vm.Waiting
import it.unibo.tuprolog.argumentation.core.runtime.context.IdSet
import it.unibo.tuprolog.argumentation.core.runtime.context.contextLibrary
import it.unibo.tuprolog.solve.Signature

/**
 * The graph [nodes] solvers of a session build together, each with its part of the rules, by the pipeline as it is
 * (structuredWhole::buildGraph): none of them knows of the others. The graph store does it all (see the module
 * graphStore): what a node saves is its segment, published when its argument builder asks what is new; the segments of
 * the others are merged into its store then, their conclusions new for its rules; and when it has nothing new, it waits
 * for another segment, or for every node to have nothing new either, which ends the arguments. While they build the
 * graph a node goes through its own arguments (the attacks on them it finds) and looks up those of every node. The
 * graph built, each node has all of it.
 *
 * Set as [it.unibo.tuprolog.argumentation.core.engine.vm.SolverSession.shared] of the session of the nodes, one for each
 * graph. The nodes run on threads of their own: a node waits for the others.
 */
@InternalArg2pApi
class SharedGraph(
    private val nodes: Int,
) {
    private val waiting = Waiting()

    // the segments published, in order, and by which node
    private val segments = ArrayList<Pair<Node, GraphSegment>>()

    // the nodes with nothing new, waiting; whether the arguments are over; the nodes done with the graph
    private var idle = 0
    private var argumentsOver = false
    private var settled = 0

    private val members = HashMap<Any, Node>()

    internal class Node {
        // the segments read, where its own next one starts, the arguments merged from others (not its own)
        var read = 0
        var mark = GraphMark.START
        val merged = IdSet(16)
        var building = true
    }

    private fun node(key: Any): Node = waiting.locked { members.getOrPut(key) { Node() } }

    private fun publish(
        call: NativeCall,
        node: Node,
    ) {
        val (segment, next) = call.graphSegmentSince(node.mark)
        node.mark = next
        if (segment.size == 0 && segment.supports.isEmpty() && segment.attacks.isEmpty()) return
        waiting.locked {
            segments += node to segment
            waiting.wakeAll()
        }
    }

    // (under the lock) the segments of the others not read yet, read
    private fun unread(node: Node): List<GraphSegment> {
        val found = ArrayList<GraphSegment>()
        for (i in node.read until segments.size) {
            val (by, segment) = segments[i]
            if (by !== node) found += segment
        }
        node.read = segments.size
        return found
    }

    // [taken] merged into the store of the node, its conclusions new under [tag] if any: how many arguments were new here
    private fun merge(
        call: NativeCall,
        node: Node,
        taken: List<GraphSegment>,
        tag: String?,
    ): Int {
        var added = 0
        for (segment in taken) {
            val ids = call.mergeGraphSegment(segment, tag)
            node.merged.addAll(ids)
            added += ids.size
        }
        // (the merged facts are not published again: the next segment starts after them)
        node.mark = call.graphMarkNow()
        return added
    }

    /** What the argument builder of a node is new about, under [tag]: see the class. [nothingNew]: the node has nothing new */
    internal fun share(
        call: NativeCall,
        key: Any,
        tag: String,
        nothingNew: Boolean,
    ) {
        val node = node(key)
        publish(call, node)
        val ready = waiting.locked { unread(node) }
        if (merge(call, node, ready, tag) > 0 || !nothingNew) return
        while (true) {
            val taken =
                waiting.locked {
                    if (argumentsOver) return@locked null
                    idle++
                    if (idle == nodes) {
                        argumentsOver = true
                        waiting.wakeAll()
                    }
                    while (!argumentsOver && segments.size == node.read) waiting.await()
                    idle--
                    unread(node)
                } ?: return
            if (merge(call, node, taken, tag) > 0) return
            if (taken.isEmpty() && waiting.locked { argumentsOver }) return
        }
    }

    /** The graph of the node passed on, and every node's merged once all of them have passed theirs: see the class */
    internal fun settle(
        call: NativeCall,
        key: Any,
    ) {
        val node = node(key)
        publish(call, node)
        val taken =
            waiting.locked {
                settled++
                waiting.wakeAll()
                while (settled < nodes) waiting.await()
                unread(node)
            }
        merge(call, node, taken, null)
        node.building = false
    }

    /** The arguments the node saved itself, while the graph is being built; null after (and for a node of no shared graph) */
    internal fun own(
        call: NativeCall,
        key: Any,
    ): IntArray? {
        val node = waiting.locked { members[key] } ?: return null
        if (!node.building) return null
        val saved =
            call.contextLibrary
                .store()!!
                .ids("arg")
                .toIntArray()
        return saved.filter { it !in node.merged }.toIntArray()
    }

    internal companion object {
        fun of(call: NativeCall): SharedGraph? =
            call.contextLibrary
                .store()
                ?.encoding
                ?.session
                ?.shared as? SharedGraph
    }
}

/** graphStore::graph_share(+Tag, +NothingNew): see [SharedGraph.share]; nothing outside a shared graph */
internal object GraphShare : AbstractNativePrimitive(Signature("graph_share", 2)) {
    override fun call(call: NativeCall): Int {
        val graph = SharedGraph.of(call) ?: return NativeCall.CONTINUE
        graph.share(call, call.contextLibrary, call.atomName(call.arg(0)), call.atomName(call.arg(1)) == "true")
        return NativeCall.CONTINUE
    }
}

/** graphStore::graph_settle: see [SharedGraph.settle]; nothing outside a shared graph */
internal object GraphSettle : AbstractNativePrimitive(Signature("graph_settle", 0)) {
    override fun call(call: NativeCall): Int {
        SharedGraph.of(call)?.settle(call, call.contextLibrary)
        return NativeCall.CONTINUE
    }
}

/** graphStore::graph_own(-Ids): the arguments the node saved itself while a shared graph is built; fails otherwise */
internal object GraphOwn : AbstractNativePrimitive(Signature("graph_own", 1)) {
    override fun call(call: NativeCall): Int {
        val own = SharedGraph.of(call)?.own(call, call.contextLibrary) ?: return NativeCall.FAIL
        return call.replyWord(0, call.list(IntArray(own.size) { call.int(own[it]) }))
    }
}
