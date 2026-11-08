package it.unibo.tuprolog.argumentation.core.primitives.graph

import it.unibo.tuprolog.argumentation.core.runtime.context.contextLibrary
import it.unibo.tuprolog.argumentation.engine.vm.ClauseStore
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.listArg
import it.unibo.tuprolog.argumentation.engine.words.IdSet
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError

// The graph kept in the active context and its labels, for the native primitives: the Kotlin side of the module
// graphStore, reading the same facts, by the identifiers of the arguments

/** The arguments of the graph of a branch, `arg(Id)`, newest first */
internal class GraphArguments(
    stored: List<ClauseStore.Stored>,
) {
    /** The identifiers of the arguments */
    val ids: IntArray =
        IntArray(stored.size) { i -> stored[i].record.idAt(0) }

    private val positions =
        IntArray((ids.maxOrNull() ?: -1) + 1) { -1 }.also { positions ->
            for (i in ids.indices) positions[ids[i]] = i
        }

    /** The place of the argument with identifier [id], -1 if there is none */
    fun position(id: Int): Int = if (id < positions.size) positions[id] else -1
}

/** The arguments of the graph of the active branch, read once until they change */
internal fun NativeCall.graphArguments(): GraphArguments = contextLibrary.derived("arg/1", "arguments") { GraphArguments(it) }

/**
 * The attack graph of the active branch, the saved attacks and those they extend to (see [ExpandedAttacks]): built
 * once until they change (the native labellings read it at each step of their search)
 */
internal fun NativeCall.attackGraph(): AttackGraph = expandedAttacks().graph

/** The identifiers of the arguments labelled with [label] (in, out or und), in the given or the active branch */
internal fun NativeCall.labelledIds(
    label: String,
    branch: Int? = null,
): IdSet = contextLibrary.store(branch)?.ids(idFact(label)) ?: IdSet.NONE

private fun idFact(label: String): String =
    when (label) {
        "in" -> "inId"
        "out" -> "outId"
        "und" -> "undId"
        else -> "${label}Id"
    }

/** The arguments at the given [positions] of [arguments] labelled with [label], by themselves and by their identifiers */
internal fun NativeCall.labelArguments(
    arguments: GraphArguments,
    label: String,
    positions: IntArray,
) {
    val context = contextLibrary
    val store = context.store()!!
    val idFact = idFact(label)
    for (position in positions) {
        val id = arguments.ids[position]
        store.assertNamed(label, context.record(id)!!)
        store.assertId(idFact, id)
    }
}

/** The arguments at the given [positions] of [arguments] no longer labelled with [label], the others keeping their order */
internal fun NativeCall.unlabelArguments(
    arguments: GraphArguments,
    label: String,
    positions: IntArray,
) {
    if (positions.isEmpty()) return
    val context = contextLibrary
    val store = context.store()!!
    val ids = IdSet(positions.size)
    for (position in positions) ids.add(arguments.ids[position])
    store.retractNamed(label, positions.map { context.record(arguments.ids[it])!! })
    store.retractIds(idFact(label), ids)
}

/** The argument [i], a list of identifiers (a type error otherwise, from the primitive of [signature]) */
internal fun NativeCall.idsArg(
    i: Int,
    signature: Signature,
): IntArray {
    val items = listArg(i, signature)
    return IntArray(items.size) { index ->
        val item = items[index]
        if (kind(item) != NativeCall.INT) throw TypeError.forArgument(context, signature, TypeError.Expected.INTEGER, term(item), i)
        intValue(item).toInt()
    }
}

/** The list of the identifiers [ids] */
internal fun NativeCall.idList(ids: IntArray): Int = list(IntArray(ids.size) { int(ids[it]) })
