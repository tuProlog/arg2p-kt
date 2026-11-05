package it.unibo.tuprolog.argumentation.core.engine.vm

/**
 * The words of the machine (spike): a term is an `Int`, its three low bits the tag, the rest the payload. A heap is
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

    const val INT_MIN = -(1 shl 28)
    const val INT_MAX = (1 shl 28) - 1

    fun fitsInt(value: Long): Boolean = value in INT_MIN..INT_MAX
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
