package it.unibo.tuprolog.argumentation.core.model

import kotlin.js.JsExport
import kotlin.js.JsName

/** Lists of positions, one for each argument, in a single array: those of the argument `p` are at `offsets[p]` to `offsets[p + 1]` */
@JsExport
class CompactLists internal constructor(
    val offsets: IntArray,
    val items: IntArray,
) {
    /** The list of the argument at [position] */
    fun of(position: Int): IntArray = items.copyOfRange(offsets[position], offsets[position + 1])
}

/**
 * All that a [Graph] tells, as arrays, with no object for an argument, an attack or a support: what a graph too large for [Graph]
 * is read as. It holds no reference to the solver, and is read at any time.
 *
 * An argument is a position, from 0 to [size], in every array, in the order of the store. What is text is a position in [texts],
 * where each text is once: the conclusion of the argument `p` is `texts[conclusions[p]]`.
 *
 * Attacks and supports are parallel arrays of positions of arguments: the attack `i` is of [attackers]`[i]` on [targets]`[i]`, on
 * its sub-argument [on]`[i]`, of type [attackTypes]`[`[attackTypeOf]`[i]]`, and, as in [Graph], an attack on an argument is one on
 * every argument built on it. The argument [supporters]`[i]` supports [supported]`[i]`.
 */
@JsExport
class CompactGraph internal constructor(
    val texts: Array<String>,
    val conclusions: IntArray,
    val topRules: IntArray,
    val rules: CompactLists,
    val groundings: CompactLists,
    val defeasibleRules: CompactLists,
    val defeasiblePremises: CompactLists,
    val lastDefeasibleRules: CompactLists,
    /** The label of each argument, a position in [labelNames] */
    val labels: ByteArray,
    val attackers: IntArray,
    val targets: IntArray,
    val on: IntArray,
    val attackTypeOf: IntArray,
    val attackTypes: Array<String>,
    val supporters: IntArray,
    val supported: IntArray,
    // The conclusions as tuProlog writes them ('-'(a)), by which the arguments are ordered to be named (see order)
    private val orderedBy: IntArray,
) {
    /** How many arguments */
    val size: Int get() = conclusions.size

    /** What [labels] tell */
    val labelNames: Array<String> get() = arrayOf(NO_LABEL, IN, OUT, UND)

    /** The label of the argument at [position]: in, out, und, or na for an argument the labelling does not label */
    fun label(position: Int): String = labelNames[labels[position].toInt()]

    /** The arguments labelled [label] (in, out, und) */
    fun labelled(label: String): IntArray {
        val code = labelNames.indexOf(label).toByte()
        val found = IntArray(labels.count { it == code })
        var n = 0
        for (position in labels.indices) if (labels[position] == code) found[n++] = position
        return found
    }

    /** For each `n`, the argument named `An` in the graph, as [Graph] names them; computed once */
    val order: IntArray by lazy(LazyThreadSafetyMode.NONE) {
        val supportCount = IntArray(size)
        supported.forEach { supportCount[it]++ }
        (0 until size)
            .sortedWith(
                compareBy(
                    { supportCount[it] },
                    { rules.offsets[it + 1] - rules.offsets[it] },
                    { texts[orderedBy[it]] },
                    { texts[topRules[it]] },
                ),
            ).toIntArray()
    }

    private val names: IntArray by lazy(LazyThreadSafetyMode.NONE) {
        IntArray(size).also { names -> order.forEachIndexed { n, position -> names[position] = n } }
    }

    private val supportsOf: Map<Int, List<Int>> by lazy(LazyThreadSafetyMode.NONE) {
        supported.indices.groupBy({ supported[it] }, { supporters[it] })
    }

    /** The name of the argument at [position]: A0, A1, ... */
    fun identifier(position: Int): String = "A${names[position]}"

    /** The argument at [position] written as `A2 : A0,r1 : c` */
    fun descriptor(position: Int): String {
        val top = texts[topRules[position]]
        val body =
            if (top == NO_RULE) {
                rules.of(position).firstOrNull()?.let { texts[it] } ?: ""
            } else {
                (supportsOf[position].orEmpty().map { identifier(it) } + top).joinToString(",")
            }
        return "${identifier(position)} : $body : ${texts[conclusions[position]]}"
    }

    /** The argument at [position] as an object, with its name and no supports (see [supporters]): for a few of them */
    @JsName("argument")
    fun argument(position: Int): Argument {
        val named = { lists: CompactLists -> lists.of(position).map { texts[it] } }
        return Argument(
            named(rules),
            texts[topRules[position]],
            texts[conclusions[position]],
            named(groundings),
            named(defeasibleRules),
            named(defeasiblePremises),
            named(lastDefeasibleRules),
        ).also { it.identifier = identifier(position) }
    }

    internal companion object {
        const val NO_LABEL = "na"
        const val IN = "in"
        const val OUT = "out"
        const val UND = "und"
        const val NO_RULE = "none"
    }
}
