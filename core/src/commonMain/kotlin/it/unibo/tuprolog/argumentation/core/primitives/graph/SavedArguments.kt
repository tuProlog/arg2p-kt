package it.unibo.tuprolog.argumentation.core.primitives.graph

import it.unibo.tuprolog.argumentation.InternalArg2pApi
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.PortableRecords
import it.unibo.tuprolog.argumentation.core.engine.Record
import it.unibo.tuprolog.argumentation.core.engine.vm.Words
import it.unibo.tuprolog.argumentation.core.engine.vm.payload
import it.unibo.tuprolog.argumentation.core.engine.vm.tag
import it.unibo.tuprolog.argumentation.core.model.terms.ArgumentTerm
import it.unibo.tuprolog.argumentation.core.runtime.context.ClauseStore
import it.unibo.tuprolog.argumentation.core.runtime.context.Context
import it.unibo.tuprolog.argumentation.core.runtime.context.IdSet
import it.unibo.tuprolog.argumentation.core.runtime.context.contextLibrary
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term

/**
 * [arguments], ground, saved as `graphStore::saveArguments(Tag, Arguments)` saves them, straight in the store of the active
 * branch: the identifier of each. What the catalog of the parallel solver does with all the workers built, which on the heap,
 * and with a resolution for each, takes more than building them.
 */
@InternalArg2pApi
fun NativeCall.saveBuiltArguments(
    tag: String,
    arguments: Collection<Term>,
): IntArray {
    val context = contextLibrary
    val store = context.store()!!
    val saved = store.ids("arg").copy()
    val noted = Atom.of(tag)
    val ids = IntArray(arguments.size)
    arguments.forEachIndexed { i, argument ->
        val id = context.identifier(Struct.of("argument", argument))
        ids[i] = id
        if (saved.add(id)) {
            val parts = ArgumentTerm.parts(argument)!!
            store.assertA(context.record(id)!!)
            store.assertId("arg", id)
            val info = ArgumentTerm.info(parts.lastDefeasibleRules, parts.defeasibleRules, parts.defeasiblePremises)
            val infoId = context.identifier(Struct.of("info", info))
            store.assertA(Struct.of("argInfo", Integer.of(id), Integer.of(infoId)))
            store.assertA(Struct.of("newConc", noted, parts.conclusion))
        }
    }
    return ids
}

/**
 * The arguments saved in the active branch after the first [from] of them (see [argumentsSavedSince]), as the nodes of
 * the parallel solver pass them on: the identifier of each ([ids], in the order they were saved, the supporters of an
 * argument before it), three records for each in [records] (the argument `argument(A)`, its info `info(I)`, its
 * conclusion `[C]`, as the store keeps them: nothing decoded), the identifiers of the saved arguments supporting it,
 * newest first (as `graphStore::savedAfter/4` gives them), and how many arguments are saved ([saved], the [from] of the
 * next call).
 */
@InternalArg2pApi
class ExportedArguments(
    val ids: IntArray,
    val records: PortableRecords,
    val supporters: Array<IntArray>,
    val saved: Int,
)

/** The identifiers of the arguments saved in the active branch after the first [from] of them, and how many are saved */
@InternalArg2pApi
fun NativeCall.argumentIdsSavedSince(from: Int): Pair<IntArray, Int> = contextLibrary.store()!!.idsFrom("arg", from)

/**
 * The arguments saved in the active branch after the first [from] of them, by the order they were saved: an argument is
 * never taken out of a graph, so those saved since a count taken before. Not by their identifiers, which the contexts of
 * a session share: an argument built after another node gave it an identifier has an older one.
 */

@InternalArg2pApi
fun NativeCall.argumentsSavedSince(from: Int): ExportedArguments {
    val context = contextLibrary
    val store = context.store()!!
    val saved = store.ids("arg")
    val (ids, count) = store.idsFrom("arg", from)
    val new = IdSet(ids.size).addAll(ids)
    val infos = context.infoOfArguments()
    // the supporters of each, newest first, those saved
    val pairs = context.supportPairs()
    val supporters = HashMap<Int, MutableList<Int>>()
    for (i in 0 until pairs.size / 2) {
        val (supporter, supported) = pairs[2 * i] to pairs[2 * i + 1]
        if (supported in new && supporter in saved) supporters.getOrPut(supported) { mutableListOf() } += supporter
    }
    val records = ArrayList<Record>(ids.size * 3)
    for (id in ids) {
        val argument = context.record(id)!!
        records += argument
        records += context.record(infos.getValue(id))!!
        records += argument.sub(conclusionWord(argument))
    }
    return ExportedArguments(ids, PortableRecords.of(records), Array(ids.size) { supporters[ids[it]]?.toIntArray() ?: IntArray(0) }, count)
}

/**
 * The arguments of [records] at [arguments] (the indices of the arguments, three records each as [argumentsSavedAfter]
 * gives them; all of them by default), saved in the store of the active branch as `graphStore::saveArguments(Tag,
 * Arguments)` saves them (their conclusions noted as new under [tag]), those not saved already: the identifier of each,
 * in the order of [arguments]. No term is made of them.
 */
@InternalArg2pApi
fun NativeCall.saveArgumentRecords(
    tag: String,
    records: PortableRecords,
    arguments: IntArray = IntArray(records.size / 3) { it },
): IntArray {
    val context = contextLibrary
    val store = context.store()!!
    val program = store.encoding ?: return saveBuiltArguments(tag, arguments.map { records.term(3 * it).castToStruct()[0] })
    val read = records.records(program, IntArray(arguments.size * 3) { 3 * arguments[it / 3] + it % 3 })
    val saved = store.ids("arg").copy()
    val argInfo = program.functorWord("argInfo", 2)
    val newConc = program.functorWord("newConc", 2)
    val noted = Record.of(Atom.of(tag), program)
    val ids = IntArray(read.size / 3)
    for (i in ids.indices) {
        val id = context.identifier(read[3 * i])
        ids[i] = id
        if (saved.add(id)) {
            store.assertA(context.record(id)!!)
            store.assertId("arg", id)
            val infoId = context.identifier(read[3 * i + 1])
            store.assertA(
                Record.compound(program, argInfo, listOf(Record.of(Integer.of(id), program), Record.of(Integer.of(infoId), program))),
            )
            store.assertA(Record.compound(program, newConc, listOf(noted, read[3 * i + 2])))
        }
    }
    return ids
}

// The word of the conclusion of `argument([Rules, TopRule, [Claim], Body, Info])`: the third element of its list
internal fun conclusionWord(argument: Record): Int {
    var cell = payload(argument.words[payload(argument.root) + 1])
    repeat(2) { cell = payload(argument.words[cell + 1]) }
    return argument.words[cell]
}

// The info of each saved argument, by identifier: argInfo(Id, InfoId)
private fun Context.infoOfArguments(): Map<Int, Int> =
    derived("argInfo/2", "infoOf") { stored ->
        stored.associate { idAt(it, 0) to idAt(it, 1) }
    }

private fun idAt(
    stored: ClauseStore.Stored,
    i: Int,
): Int {
    val record = stored.record
    val word = record.words[payload(record.head) + 1 + i]
    return if (tag(word) == Words.INT) payload(word) else record.program!!.bigInteger(payload(word)).toInt()
}
