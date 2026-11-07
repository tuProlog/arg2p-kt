package it.unibo.tuprolog.argumentation.core.primitives.graph

import it.unibo.tuprolog.argumentation.InternalArg2pApi
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.Record
import it.unibo.tuprolog.argumentation.core.engine.vm.payload
import it.unibo.tuprolog.argumentation.core.runtime.context.contextLibrary
import it.unibo.tuprolog.core.Atom

/**
 * What a node of a session added to the graph of its active branch since a [GraphMark]: the facts of the graph
 * predicates (`arg/1`, `argInfo/2`, `supportId/2`, `attackId/4`) as its store keeps them. The identifiers in them are
 * those of the session, and the records are in its words: another node of the session merges them into its own store
 * ([mergeGraphSegment]) as they are, no record copied or encoded again, its own indexes taking them.
 */
@InternalArg2pApi
class GraphSegment internal constructor(
    // the identifiers of the arguments saved, in the order they were
    internal val arguments: IntArray,
    internal val infos: List<Record>,
    internal val supports: List<Record>,
    internal val attacks: List<Record>,
) {
    /** How many arguments the segment holds */
    val size: Int get() = arguments.size
}

/** How many facts of each graph predicate the store of a node held: where the next [GraphSegment] starts */
@InternalArg2pApi
class GraphMark internal constructor(
    internal val arguments: Int,
    internal val infos: Int,
    internal val supports: Int,
    internal val attacks: Int,
) {
    companion object {
        /** Before any fact */
        val START = GraphMark(0, 0, 0, 0)
    }
}

/**
 * The graph added to the active branch since [mark], and the mark after it. The facts are taken by the order they were
 * stored in: an argument is never taken out of a graph, and attacks only by the graph builder of the node, before it
 * passes its graph on.
 */
@InternalArg2pApi
fun NativeCall.graphSegmentSince(mark: GraphMark): Pair<GraphSegment, GraphMark> {
    val store = contextLibrary.store()!!
    val (arguments, argumentCount) = store.idsFrom("arg", mark.arguments)
    val (infos, infoCount) = store.recordsFrom("argInfo", 2, mark.infos)
    val (supports, supportCount) = store.recordsFrom("supportId", 2, mark.supports)
    val (attacks, attackCount) = store.recordsFrom("attackId", 4, minOf(mark.attacks, store.count("attackId", 4)))
    return GraphSegment(arguments, infos, supports, attacks) to GraphMark(argumentCount, infoCount, supportCount, attackCount)
}

/** Where the next segment of the store of the active branch starts: after all it holds */
@InternalArg2pApi
fun NativeCall.graphMarkNow(): GraphMark {
    val store = contextLibrary.store()!!
    return GraphMark(store.count("arg", 1), store.count("argInfo", 2), store.count("supportId", 2), store.count("attackId", 4))
}

/**
 * [segment], of another node of the session, merged into the store of the active branch: the arguments not saved here
 * yet (their records `argument(A)`, as the identifiers of the session hold them, and `arg/1`, `argInfo/2`), the
 * conclusion of each noted as new under [tag] (`newConc/2`, for the rules here to build on it) if not null, and the
 * supports and attacks not here yet. The identifiers of the arguments new here, in the order of the segment.
 */
@InternalArg2pApi
fun NativeCall.mergeGraphSegment(
    segment: GraphSegment,
    tag: String?,
): IntArray {
    val context = contextLibrary
    val store = context.store()!!
    val program = store.encoding!!
    val saved = store.ids("arg").copy()
    val infoOf = HashMap<Int, Record>(segment.infos.size)
    for (info in segment.infos) infoOf[payload(info.words[payload(info.root) + 1])] = info
    val newConc = program.functorWord("newConc", 2)
    val noted = tag?.let { Record.of(Atom.of(it), program) }
    val added = ArrayList<Int>()
    for (id in segment.arguments) {
        if (!saved.add(id)) continue
        val argument = context.record(id)!!
        store.assertA(argument)
        store.assertId("arg", id)
        infoOf[id]?.let { store.assertA(it) }
        if (noted != null) store.assertA(Record.compound(program, newConc, listOf(noted, argument.sub(conclusionWord(argument)))))
        added += id
    }
    for (fact in segment.supports + segment.attacks) {
        if (store.holds(fact)) continue
        store.assertA(fact)
    }
    return added.toIntArray()
}
