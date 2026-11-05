package it.unibo.tuprolog.argumentation.core.model.mining

import it.unibo.tuprolog.argumentation.core.engine.Record
import it.unibo.tuprolog.argumentation.core.engine.RecordKey
import it.unibo.tuprolog.argumentation.core.engine.vm.Words
import it.unibo.tuprolog.argumentation.core.engine.vm.payload
import it.unibo.tuprolog.argumentation.core.engine.vm.tag
import it.unibo.tuprolog.argumentation.core.model.CompactGraph
import it.unibo.tuprolog.argumentation.core.model.CompactLists
import it.unibo.tuprolog.argumentation.core.model.terms.TermValues
import it.unibo.tuprolog.argumentation.core.primitives.graph.expandedAttacks
import it.unibo.tuprolog.argumentation.core.primitives.graph.supportPairs
import it.unibo.tuprolog.argumentation.core.runtime.context.Context
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.Solver

/** The graph of the active context branch as arrays (see [CompactGraph]) */
fun Solver.compactGraph(): CompactGraph = compactGraph(activeBranch())

/**
 * The graph of the context branch [branch] as arrays (see [CompactGraph]), read off the words of the store: nothing is resolved,
 * and of each text only the first occurrence becomes a term
 */
fun Solver.compactGraph(branch: Int): CompactGraph {
    val context = Context.of(libraries) ?: throw IllegalStateException("A solver with no context of arg2p")
    val store = context.store(branch) ?: throw IllegalArgumentException("No context branch $branch")
    val ids = store.ids("arg").toIntArray()
    val positions = IntArray((ids.maxOrNull() ?: -1) + 1) { -1 }
    ids.forEachIndexed { position, id -> positions[id] = position }
    val position = { id: Int -> if (id < positions.size) positions[id] else -1 }

    val texts = Texts()
    val written = Interned { texts.of(it.toString()) }
    val concluded = Interned { texts.of(TermValues.format(it)) }
    val ordered = Interned { texts.of(it.toString()) }
    val conclusions = IntArray(ids.size)
    val orderedBy = IntArray(ids.size)
    val topRules = IntArray(ids.size)
    val rules = ListsBuilder(ids.size)
    val groundings = ListsBuilder(ids.size)
    val defeasibleRules = ListsBuilder(ids.size)
    val defeasiblePremises = ListsBuilder(ids.size)
    val lastDefeasibleRules = ListsBuilder(ids.size)
    val parts = IntArray(5)
    val infoParts = IntArray(3)
    ids.forEachIndexed { at, id ->
        val record = context.record(id)!!
        val words = record.words
        // argument([Rules, TopRule, [Claim], Body, [LastDefeasibleRules, DefeasibleRules, DefeasiblePremises]])
        items(words, words[payload(record.head) + 1], parts)
        items(words, parts[4], infoParts)
        rules.add(record, parts[0], written)
        topRules[at] = written.of(record, parts[1])
        val conclusion = parts[2]
        val claim = if (tag(conclusion) == Words.LIST) words[payload(conclusion)] else 0
        val single = tag(conclusion) == Words.LIST && tag(words[payload(conclusion) + 1]) != Words.LIST
        conclusions[at] = if (single) concluded.of(record, claim) else texts.of("null")
        orderedBy[at] = if (single) ordered.of(record, claim) else conclusions[at]
        groundings.add(record, parts[3], written)
        defeasibleRules.add(record, infoParts[1], written)
        defeasiblePremises.add(record, infoParts[2], written)
        lastDefeasibleRules.add(record, infoParts[0], written)
    }

    val labels = ByteArray(ids.size)
    listOf("inId" to 1, "outId" to 2, "undId" to 3).forEach { (fact, code) ->
        store.ids(fact).toIntArray().forEach { id -> position(id).takeIf { it >= 0 }?.let { labels[it] = code.toByte() } }
    }

    val attacks =
        context.expandedAttacks(branch).attacks.filter {
            position(it.attacker) >= 0 &&
                position(it.attacked) >= 0 &&
                position(it.on) >= 0
        }
    val types = LinkedHashMap<String, Int>()
    val attackTypeOf = IntArray(attacks.size) { i -> attacks[i].type.toString().let { types.getOrPut(it) { types.size } } }

    val pairs = context.supportPairs(branch)
    val kept = (0 until pairs.size / 2).filter { position(pairs[2 * it]) >= 0 && position(pairs[2 * it + 1]) >= 0 }
    return CompactGraph(
        texts.toArray(),
        conclusions,
        topRules,
        rules.build(),
        groundings.build(),
        defeasibleRules.build(),
        defeasiblePremises.build(),
        lastDefeasibleRules.build(),
        labels,
        IntArray(attacks.size) { position(attacks[it].attacker) },
        IntArray(attacks.size) { position(attacks[it].attacked) },
        IntArray(attacks.size) { position(attacks[it].on) },
        attackTypeOf,
        types.keys.toTypedArray(),
        IntArray(kept.size) { position(pairs[2 * kept[it]]) },
        IntArray(kept.size) { position(pairs[2 * kept[it] + 1]) },
        orderedBy,
    )
}

// The first items of the list [list0] in [into]
private fun items(
    words: IntArray,
    list0: Int,
    into: IntArray,
) {
    var list = list0
    var i = 0
    while (i < into.size && tag(list) == Words.LIST) {
        val cell = payload(list)
        into[i++] = words[cell]
        list = words[cell + 1]
    }
}

// Each text once
private class Texts {
    private val all = ArrayList<String>()
    private val known = HashMap<String, Int>()

    fun of(text: String): Int =
        known.getOrPut(text) {
            all.add(text)
            all.size - 1
        }

    fun toArray(): Array<String> = all.toTypedArray()
}

// The position of the text of a term of a record, the term read once: a symbol by its word, anything else by its key
private class Interned(
    private val text: (Term) -> Int,
) {
    private val bySymbol = HashMap<Int, Int>()
    private val byKey = HashMap<RecordKey, Int>()

    fun of(
        record: Record,
        word: Int,
    ): Int =
        if (tag(word) == Words.ATOM || tag(word) == Words.INT) {
            bySymbol.getOrPut(word) { text(record.termAt(word)) }
        } else {
            byKey.getOrPut(record.keyAt(word)) { text(record.termAt(word)) }
        }
}

// The lists of the arguments, in order, in one array
private class ListsBuilder(
    size: Int,
) {
    private val offsets = IntArray(size + 1)
    private var items = IntArray(maxOf(size, 8))
    private var argument = 0

    fun add(
        record: Record,
        list0: Int,
        interned: Interned,
    ) {
        val words = record.words
        var list = list0
        var n = offsets[argument]
        while (tag(list) == Words.LIST) {
            val cell = payload(list)
            if (n == items.size) items = items.copyOf(n * 2)
            items[n++] = interned.of(record, words[cell])
            list = words[cell + 1]
        }
        offsets[++argument] = n
    }

    fun build() = CompactLists(offsets, items.copyOf(offsets[argument]))
}
