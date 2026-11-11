package it.unibo.tuprolog.argumentation.engine.vm.program

import it.unibo.tuprolog.argumentation.engine.words.Words
import it.unibo.tuprolog.argumentation.engine.words.word
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Real
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import org.gciatto.kt.math.BigDecimal
import org.gciatto.kt.math.BigInteger

/**
 * The numbers no word holds, as compound terms of the machine: an integer beyond a small one `'$int'(Sign, Chunk...)`, a float
 * `'$float'(Scale, Sign, Chunk...)` (its unscaled value and scale, trailing zeros stripped), the chunks [CHUNK_BITS] bits of the
 * magnitude each, the most significant first, none leading at 0, all small integers. Equal values are the same term: unified,
 * compared and hashed as any compound, and on the heap, collected with it, rather than in a table that would only grow (a loop
 * over large integers kept each of them). Type tests, term order, arithmetic and the conversion to tuProlog terms read them as
 * numbers (see [Program.numberTag]).
 */
internal object Numbers {
    const val INT_NAME = "\$int"
    const val FLOAT_NAME = "\$float"
    const val CHUNK_BITS = 24
    private const val CHUNK_MASK = (1 shl CHUNK_BITS) - 1

    // The chunks of a non-negative Long, most significant first, none leading at 0
    fun chunksOf(magnitude: Long): IntArray {
        var n = 0
        var m = magnitude
        while (m != 0L) {
            n++
            m = m ushr CHUNK_BITS
        }
        val chunks = IntArray(maxOf(n, 1))
        m = magnitude
        for (i in chunks.indices.reversed()) {
            chunks[i] = (m and CHUNK_MASK.toLong()).toInt()
            m = m ushr CHUNK_BITS
        }
        return chunks
    }

    // The chunks of the magnitude of [value]
    fun chunksOf(value: BigInteger): IntArray {
        val magnitude = value.absoluteValue
        if (magnitude.bitLength < 63) return chunksOf(magnitude.toLong())
        val n = (magnitude.bitLength + CHUNK_BITS - 1) / CHUNK_BITS
        val mask = BigInteger.of(CHUNK_MASK)
        return IntArray(n) { i -> (magnitude shr (CHUNK_BITS * (n - 1 - i))).and(mask).toInt() }
    }

    /** The Long of a sign and chunks, null if it takes more than 63 bits */
    fun longOf(
        sign: Int,
        chunks: IntArray,
    ): Long? {
        var m = 0L
        for (c in chunks) {
            if (m ushr (63 - CHUNK_BITS) != 0L) return null
            m = (m shl CHUNK_BITS) or c.toLong()
        }
        return if (sign < 0) -m else m
    }

    /** The integer of a sign and chunks */
    fun bigOf(
        sign: Int,
        chunks: IntArray,
    ): BigInteger {
        longOf(sign, chunks)?.let { return BigInteger.of(it) }
        var m = BigInteger.ZERO
        for (c in chunks) m = (m shl CHUNK_BITS) + BigInteger.of(c)
        return if (sign < 0) -m else m
    }

    // The canonical form of a float: its value with no trailing zeros
    private fun canonical(value: BigDecimal): BigDecimal = if (value.signum == 0) BigDecimal.ZERO else value.stripTrailingZeros()

    /** The float of a scale, a sign and chunks, written as tuProlog writes a float (with a decimal point) */
    fun realOf(
        scale: Int,
        sign: Int,
        chunks: IntArray,
    ): Real {
        var value = BigDecimal.of(bigOf(sign, chunks)).scaleByPowerOfTen(-scale)
        if (value.scale < 0) value = value.setScale(0)
        // (no decimal point in its text: an integer, or one digit in scientific notation, below an exponent of -6)
        if (value.scale == 0 || (value.precision == 1 && value.precision - value.scale - 1 < -6)) value = value.setScale(value.scale + 1)
        return Real.of(value)
    }

    /**
     * The words of [value] as [write] puts them, a block of cells given by [alloc] (the address of the first): the word of the
     * term, a small integer if it fits one
     */
    inline fun write(
        program: Program,
        value: Long,
        alloc: (Int) -> Int,
        put: (Int, Int) -> Unit,
    ): Int {
        // (its range tested here once: no smallWord, that tests it again)
        if (Words.fitsInt(value)) return word(Words.INT, value.toInt())
        val sign = if (value < 0) -1 else 1
        val chunks =
            if (value == Long.MIN_VALUE) chunksOf(BigInteger.of(value)) else chunksOf(if (value < 0) -value else value)
        return block(program, INT_NAME, intArrayOf(sign), chunks, alloc, put)
    }

    /** As the other [write], for an integer of any size */
    inline fun write(
        program: Program,
        value: BigInteger,
        alloc: (Int) -> Int,
        put: (Int, Int) -> Unit,
    ): Int {
        if (value.bitLength < 63) return write(program, value.toLong(), alloc, put)
        return block(program, INT_NAME, intArrayOf(value.signum), chunksOf(value), alloc, put)
    }

    /** As the other [write], for a float */
    inline fun write(
        program: Program,
        value: Real,
        alloc: (Int) -> Int,
        put: (Int, Int) -> Unit,
    ): Int {
        val c = canonicalOf(value)
        return block(program, FLOAT_NAME, intArrayOf(c.scale, if (c.signum < 0) -1 else 1), chunksOf(c.unscaledValue), alloc, put)
    }

    // (public for the inline functions above)
    fun canonicalOf(value: Real): BigDecimal = canonical(value.value)

    inline fun block(
        program: Program,
        name: String,
        head: IntArray,
        chunks: IntArray,
        alloc: (Int) -> Int,
        put: (Int, Int) -> Unit,
    ): Int {
        val n = head.size + chunks.size
        val s = alloc(n + 1)
        put(s, program.functorWord(name, n))
        for (i in head.indices) put(s + 1 + i, program.smallWord(head[i]))
        for (i in chunks.indices) put(s + 1 + head.size + i, program.smallWord(chunks[i]))
        return word(Words.STR, s)
    }

    /** Whether [term] is a number no word holds (see [encoded]) */
    fun needsBlock(term: Term): Boolean = (term is Integer && term.value.bitLength >= 29) || term is Real

    /**
     * [term] with the numbers no word holds written as their compound terms (see [Numbers]): for the compiler, which compiles
     * a compound as it is. The term itself if it has none.
     */
    fun encoded(term: Term): Term =
        when {
            term is Integer && term.value.bitLength >= 29 -> {
                val v = term.value
                Struct.of(INT_NAME, listOf(Integer.of(v.signum)) + chunksOf(v).map { Integer.of(it) })
            }
            term is Real -> {
                val c = canonicalOf(term)
                Struct.of(
                    FLOAT_NAME,
                    listOf(Integer.of(c.scale), Integer.of(if (c.signum < 0) -1 else 1)) + chunksOf(c.unscaledValue).map { Integer.of(it) },
                )
            }
            term is Struct && term.arity > 0 -> {
                val args = term.args
                var changed = false
                val encodedArgs = args.map { a -> encoded(a).also { if (it !== a) changed = true } }
                if (changed) term.setArgs(encodedArgs) else term
            }
            else -> term
        }
}
