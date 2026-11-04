package it.unibo.tuprolog.argumentation.core.primitives.graph

import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.listArg
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.STR
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.payload
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.tag
import it.unibo.tuprolog.argumentation.core.runtime.context.ClauseStore
import it.unibo.tuprolog.argumentation.core.runtime.context.IdSet
import it.unibo.tuprolog.argumentation.core.runtime.context.contextLibrary
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError

// The graph kept in the active context and its labels, for the native primitives: the Kotlin side of the module
// graphStore, reading the same facts, by the identifiers of the arguments

/** The arguments of the graph of a branch, `arg(Id) :- argument(A)`, newest first */
internal class GraphArguments(
    stored: List<ClauseStore.Stored>,
) {
    private val clauses =
        stored.filter { clause ->
            val record = clause.record
            val body = record.body
            tag(body) == STR && record.words[payload(body)] == record.program!!.functorWord("argument", 1)
        }

    /** The identifiers of the arguments */
    val ids: IntArray =
        IntArray(clauses.size) { i ->
            val record = clauses[i].record
            payload(record.words[payload(record.head) + 1])
        }

    private val positions =
        IntArray((ids.maxOrNull() ?: -1) + 1) { -1 }.also { positions ->
            for (i in ids.indices) positions[ids[i]] = i
        }

    /** The place of the argument with identifier [id], -1 if there is none */
    fun position(id: Int): Int = if (id < positions.size) positions[id] else -1

    /** The clause of the argument at place [position] */
    fun clause(position: Int): ClauseStore.Stored = clauses[position]
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
    val store = contextLibrary.store()!!
    val idFact = idFact(label)
    for (position in positions) {
        store.assertArgumentOfBody(label, arguments.clause(position))
        store.assertId(idFact, arguments.ids[position])
    }
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
internal fun NativeCall.idList(ids: IntArray): Int = list(IntArray(ids.size) { int(ids[it].toLong()) })
