package it.unibo.tuprolog.argumentation.engine.words

/**
 * The words of the machine: a term is an `Int`, its three low bits the tag, the rest the payload. A heap is
 * an `IntArray` of such words; a compound term is the address of its cells (its functor, then its arguments); a
 * variable is a cell that refers to itself while unbound. Ints are plain in Kotlin/JS too: a 32-bit word keeps the
 * machine the same on every target.
 */
internal object Words {
    const val TAG_BITS = 3
    const val TAG_MASK = (1 shl TAG_BITS) - 1

    /** A reference to a cell (on the heap: its address; in the code of a clause: the slot of a variable) */
    const val REF = 0

    /** An atom, by its id */
    const val ATOM = 1

    /** A small integer (29 bits, signed) */
    const val INT = 2

    /** A compound term, by the address of its functor cell */
    const val STR = 3

    /** A list cell, by the address of its head (its tail follows) */
    const val LIST = 4

    /** A float, by its index in the table of floats */
    const val FLT = 5

    /** A large integer, by its index in the table of large integers */
    const val BIG = 6

    /** The functor of a compound term, by its id: the first cell of the term */
    const val FUN = 7

    /** The key of a list cell, among those of the arguments (see [wordKey]) */
    const val LIST_KEY = (0 shl TAG_BITS) or LIST

    /** The tags that pass each type test, as bits */
    val TYPE_TESTS: Map<String, Int> =
        mapOf(
            "var" to mask(REF),
            "nonvar" to (mask(REF).inv() and 0xFF),
            "atom" to mask(ATOM),
            "atomic" to mask(ATOM, INT, BIG, FLT),
            "callable" to mask(ATOM, STR, LIST),
            "compound" to mask(STR, LIST),
            "float" to mask(FLT),
            "integer" to mask(INT, BIG),
            "number" to mask(INT, BIG, FLT),
        )

    private fun mask(vararg tags: Int): Int = tags.fold(0) { mask, tag -> mask or (1 shl tag) }

    const val INT_MIN = -(1 shl 28)
    const val INT_MAX = (1 shl 28) - 1

    // (compared, not `in`: on JS the range would be an object for each call)
    fun fitsInt(value: Long): Boolean = value >= INT_MIN && value <= INT_MAX
}

@Suppress("NOTHING_TO_INLINE")
internal inline fun tag(word: Int): Int = word and Words.TAG_MASK

@Suppress("NOTHING_TO_INLINE")
internal inline fun payload(word: Int): Int = word shr Words.TAG_BITS

@Suppress("NOTHING_TO_INLINE")
internal inline fun word(
    tag: Int,
    payload: Int,
): Int = (payload shl Words.TAG_BITS) or tag

@Suppress("NOTHING_TO_INLINE")
internal inline fun ref(address: Int): Int = word(Words.REF, address)

@Suppress("NOTHING_TO_INLINE")
internal inline fun smallInt(value: Int): Int = word(Words.INT, value)

/** Whether the payload of [w] is an address: a reference, a compound term, a list cell */
@Suppress("NOTHING_TO_INLINE")
internal inline fun hasAddress(w: Int): Boolean {
    val t = tag(w)
    return t == Words.REF || t == Words.STR || t == Words.LIST
}

/** The word [w] with the address it holds moved by [by], as it is if it holds none */
@Suppress("NOTHING_TO_INLINE")
internal inline fun shifted(
    w: Int,
    by: Int,
): Int = if (hasAddress(w)) word(tag(w), payload(w) + by) else w

/** The key of the dereferenced term [d] of [words]: 0 for a variable, the functor word, [Words.LIST_KEY], the constant */
@Suppress("NOTHING_TO_INLINE")
internal inline fun wordKey(
    words: IntArray,
    d: Int,
): Int =
    when (tag(d)) {
        Words.REF -> 0
        Words.STR -> words[payload(d)]
        Words.LIST -> Words.LIST_KEY
        else -> d
    }
