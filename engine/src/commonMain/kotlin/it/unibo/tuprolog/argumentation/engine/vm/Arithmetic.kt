package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.vm.program.Numbers
import it.unibo.tuprolog.argumentation.engine.vm.program.Op
import it.unibo.tuprolog.argumentation.engine.vm.program.Program
import it.unibo.tuprolog.argumentation.engine.words.Words.BIG
import it.unibo.tuprolog.argumentation.engine.words.Words.INT
import it.unibo.tuprolog.argumentation.engine.words.Words.STR
import it.unibo.tuprolog.argumentation.engine.words.payload
import it.unibo.tuprolog.argumentation.engine.words.tag
import it.unibo.tuprolog.argumentation.engine.words.word
import kotlin.jvm.JvmField

// (no value of evaluateSmall: Int.MIN_VALUE as a value only takes the longer way)
private const val NOT_SMALL = Int.MIN_VALUE

// Up to this, a product fits in an Int
private const val SMALL_FACTOR = 46340

/**
 * The integer arithmetic of a machine on the words of its [heap]: the expressions the compiler inlines (is/2 and the
 * comparisons, the code of the E_ instructions of [Op]) and those the builtins evaluate ([eval]). Within 32 bits first, with
 * no Long (a BigInt in JavaScript), then within 64; what neither holds (a float, another function, an overflow, an error) is
 * left to the library, the expression then built as a term ([build]). The operands of an expression are read from the
 * registers and from the slots of the frame passed in (`regs`, `local`, `slots`, the first slot): this owns its stacks
 * and its functors alone.
 */
internal class Arithmetic(
    private val program: Program,
    private val heap: Heap,
) {
    /** is/2, and the comparisons by kind (see [Op.COMPARISONS]): the builtins an expression left to the library is given to */
    @JvmField val fIs = program.functor("is", 2)

    @JvmField val comparisonFunctors =
        IntArray(
            Op.C_NE + 1,
        ).also { for ((name, kind) in Op.COMPARISONS) it[kind] = program.functor(name, 2) }

    // By element (see Op.OPERATIONS): the functor of each operation, and its word for [build]
    private val functors = IntArray(Op.E_NEG + 1)
    private val functorWords = IntArray(Op.E_NEG + 1)

    init {
        for (operation in Op.OPERATIONS) {
            functors[operation.kind] = program.functor(operation.name, operation.arity)
            functorWords[operation.kind] = program.functorWord(operation.name, operation.arity)
        }
    }

    // The element of the operation of [functor], 0 for none
    private fun elementOf(functor: Int): Int {
        for (kind in Op.E_ADD..Op.E_NEG) if (functors[kind] == functor) return kind
        return 0
    }

    // The stacks: the values being computed, the words of an expression being built; whether the last evaluation could not
    private var values = LongArray(32)
    private val smallValues = IntArray(32)
    private var exprWords = IntArray(32)
    private var evalFailed = false

    // The word of the value of the expression code at [from] ([n] words): 0 for what is left to the builtin
    fun word(
        code: IntArray,
        from: Int,
        n: Int,
        regs: IntArray,
        local: IntArray,
        slots: Int,
    ): Int {
        val small = evaluateSmall(code, from, n, regs, local, slots)
        if (small != NOT_SMALL) return heap.intWord(small.toLong())
        val value = evaluate(code, from, n, regs, local, slots)
        return if (evalFailed) 0 else heap.intWord(value)
    }

    // Whether the comparison ARITH_CMP at [at] holds: 1 or 0, -1 for what is left to the builtin
    fun comparison(
        code: IntArray,
        at: Int,
        nA: Int,
        nB: Int,
        regs: IntArray,
        local: IntArray,
        slots: Int,
    ): Int {
        val smallA = evaluateSmall(code, at + 3, nA, regs, local, slots)
        if (smallA != NOT_SMALL) {
            val smallB = evaluateSmall(code, at + 4 + nA, nB, regs, local, slots)
            if (smallB != NOT_SMALL) return if (holds(code[at + 1], smallA, smallB)) 1 else 0
        }
        val a = evaluate(code, at + 3, nA, regs, local, slots)
        val b = if (evalFailed) 0L else evaluate(code, at + 4 + nA, nB, regs, local, slots)
        if (evalFailed) return -1
        return if (holds(code[at + 1], a.compareTo(b), 0)) 1 else 0
    }

    // [evaluate] within 32 bits, with no Long (a BigInt on JS): [NOT_SMALL] for an operand that is no small integer, for a
    // step out of them, or for an error: [evaluate] then says what it is
    private fun evaluateSmall(
        code: IntArray,
        from: Int,
        n: Int,
        regs: IntArray,
        local: IntArray,
        slots: Int,
    ): Int {
        val stack = smallValues
        var sp = 0
        var i = from
        val end = from + n
        while (i < end) {
            val kind = code[i]
            val operand = code[i + 1]
            i += 2
            when (kind) {
                Op.E_X, Op.E_Y -> {
                    val w = heap.dereferenced(if (kind == Op.E_X) regs[operand] else local[slots + operand])
                    if (tag(w) != INT || sp == stack.size) return NOT_SMALL
                    stack[sp++] = payload(w)
                }
                Op.E_CONST -> {
                    if (sp == stack.size) return NOT_SMALL
                    stack[sp++] = payload(operand)
                }
                Op.E_ADD -> {
                    sp--
                    val x = stack[sp - 1]
                    val y = stack[sp]
                    val r = x + y
                    if ((x xor r) and (y xor r) < 0) return NOT_SMALL
                    stack[sp - 1] = r
                }
                Op.E_SUB -> {
                    sp--
                    val x = stack[sp - 1]
                    val y = stack[sp]
                    val r = x - y
                    if ((x xor y) and (x xor r) < 0) return NOT_SMALL
                    stack[sp - 1] = r
                }
                Op.E_MUL -> {
                    sp--
                    val x = stack[sp - 1]
                    val y = stack[sp]
                    if (x < -SMALL_FACTOR || x > SMALL_FACTOR || y < -SMALL_FACTOR || y > SMALL_FACTOR) return NOT_SMALL
                    stack[sp - 1] = x * y
                }
                Op.E_IDIV -> {
                    sp--
                    val x = stack[sp - 1]
                    val y = stack[sp]
                    if (y == 0 || x == Int.MIN_VALUE) return NOT_SMALL
                    stack[sp - 1] = x / y
                }
                Op.E_MOD -> {
                    sp--
                    val y = stack[sp]
                    if (y == 0) return NOT_SMALL
                    stack[sp - 1] = stack[sp - 1].mod(y)
                }
                else -> {
                    if (stack[sp - 1] == Int.MIN_VALUE) return NOT_SMALL
                    stack[sp - 1] = -stack[sp - 1]
                }
            }
        }
        return stack[0]
    }

    // The value of the expression code at [from] ([n] words): [evalFailed] when an operand is not an integer (the
    // builtin then raises the error, or computes it)
    private fun evaluate(
        code: IntArray,
        from: Int,
        n: Int,
        regs: IntArray,
        local: IntArray,
        slots: Int,
    ): Long {
        evalFailed = false
        var sp = 0
        var i = from
        val end = from + n
        while (i < end) {
            val kind = code[i]
            val operand = code[i + 1]
            i += 2
            when (kind) {
                Op.E_X, Op.E_Y -> {
                    val w = heap.deref(if (kind == Op.E_X) regs[operand] else local[slots + operand])
                    val v =
                        when (tag(w)) {
                            INT -> payload(w).toLong()
                            STR -> {
                                val long = longOfNumber(w)
                                if (long == null) {
                                    evalFailed = true
                                    return 0
                                }
                                long
                            }
                            else -> {
                                evalFailed = true
                                return 0
                            }
                        }
                    if (sp == values.size) values = values.copyOf(sp * 2)
                    values[sp++] = v
                }
                Op.E_CONST -> {
                    if (sp == values.size) values = values.copyOf(sp * 2)
                    values[sp++] = payload(operand).toLong()
                }
                Op.E_ADD -> {
                    sp--
                    val a = values[sp - 1]
                    val b = values[sp]
                    val r = a + b
                    if (additionOverflows(a, b, r)) {
                        evalFailed = true
                        return 0
                    }
                    values[sp - 1] = r
                }
                Op.E_SUB -> {
                    sp--
                    val a = values[sp - 1]
                    val b = values[sp]
                    val r = a - b
                    if (subtractionOverflows(a, b, r)) {
                        evalFailed = true
                        return 0
                    }
                    values[sp - 1] = r
                }
                Op.E_MUL -> {
                    sp--
                    val a = values[sp - 1]
                    val b = values[sp]
                    val r = a * b
                    if (productOverflows(a, b, r)) {
                        evalFailed = true
                        return 0
                    }
                    values[sp - 1] = r
                }
                Op.E_IDIV -> {
                    sp--
                    val divisor = values[sp]
                    if (divisor == 0L || (divisor == -1L && values[sp - 1] == Long.MIN_VALUE)) {
                        evalFailed = true
                        return 0
                    }
                    values[sp - 1] = values[sp - 1] / divisor
                }
                Op.E_MOD -> {
                    sp--
                    val divisor = values[sp]
                    if (divisor == 0L) {
                        evalFailed = true
                        return 0
                    }
                    values[sp - 1] = values[sp - 1].mod(divisor)
                }
                else -> {
                    if (values[sp - 1] == Long.MIN_VALUE) {
                        evalFailed = true
                        return 0
                    }
                    values[sp - 1] = -values[sp - 1]
                }
            }
        }
        return values[0]
    }

    /**
     * The value of the integer expression [w0]: a small or large integer, or the four operations, integer division,
     * modulo and negation of such; anything else (a float, another function, an error) is left to the library
     * ([Machine.NotNative])
     */
    fun eval(w0: Int): Long {
        val w = heap.deref(w0)
        return when (tag(w)) {
            INT -> payload(w).toLong()
            STR -> {
                longOfNumber(w)?.let { return it }
                val a = payload(w)
                when (elementOf(payload(heap.cells[a]))) {
                    Op.E_ADD -> {
                        val x = eval(heap.cells[a + 1])
                        val y = eval(heap.cells[a + 2])
                        (x + y).also { if (additionOverflows(x, y, it)) throw Machine.NotNative }
                    }
                    Op.E_SUB -> {
                        val x = eval(heap.cells[a + 1])
                        val y = eval(heap.cells[a + 2])
                        (x - y).also { if (subtractionOverflows(x, y, it)) throw Machine.NotNative }
                    }
                    Op.E_MUL -> {
                        val x = eval(heap.cells[a + 1])
                        val y = eval(heap.cells[a + 2])
                        (x * y).also { if (productOverflows(x, y, it)) throw Machine.NotNative }
                    }
                    Op.E_IDIV -> {
                        val divisor = eval(heap.cells[a + 2])
                        val dividend = eval(heap.cells[a + 1])
                        if (divisor == 0L || (divisor == -1L && dividend == Long.MIN_VALUE)) throw Machine.NotNative
                        dividend / divisor
                    }
                    Op.E_MOD -> {
                        val divisor = eval(heap.cells[a + 2])
                        if (divisor == 0L) throw Machine.NotNative
                        eval(heap.cells[a + 1]).mod(divisor)
                    }
                    Op.E_NEG -> eval(heap.cells[a + 1]).let { if (it == Long.MIN_VALUE) throw Machine.NotNative else -it }
                    else -> throw Machine.NotNative
                }
            }
            else -> throw Machine.NotNative
        }
    }

    // The Long of the integer compound [w] (see Numbers): null if it is no integer, or takes more than 63 bits
    private fun longOfNumber(w: Int): Long? {
        val a = payload(w)
        val f = heap.cells[a]
        if (program.numberTag(f) != BIG) return null
        val n = program.arity(f)
        return Numbers.longOf(payload(heap.deref(heap.cells[a + 1])), IntArray(n - 1) { payload(heap.deref(heap.cells[a + 2 + it])) })
    }

    // The expression code at [from] as a term on the heap, for the builtin
    fun build(
        code: IntArray,
        from: Int,
        n: Int,
        regs: IntArray,
        local: IntArray,
        slots: Int,
    ): Int {
        var sp = 0
        var i = from
        val end = from + n
        while (i < end) {
            val kind = code[i]
            val operand = code[i + 1]
            i += 2
            when (kind) {
                Op.E_X, Op.E_Y, Op.E_CONST -> {
                    if (sp == exprWords.size) exprWords = exprWords.copyOf(sp * 2)
                    exprWords[sp++] =
                        when (kind) {
                            Op.E_X -> regs[operand]
                            Op.E_Y -> local[slots + operand]
                            else -> operand
                        }
                }
                Op.E_NEG -> {
                    val h = heap.alloc(2)
                    heap.cells[h] = functorWords[Op.E_NEG]
                    heap.cells[h + 1] = exprWords[sp - 1]
                    exprWords[sp - 1] = word(STR, h)
                }
                else -> {
                    val h = heap.alloc(3)
                    heap.cells[h] = functorWords[kind]
                    sp--
                    heap.cells[h + 1] = exprWords[sp - 1]
                    heap.cells[h + 2] = exprWords[sp]
                    exprWords[sp - 1] = word(STR, h)
                }
            }
        }
        return exprWords[0]
    }
}

private fun additionOverflows(
    a: Long,
    b: Long,
    r: Long,
): Boolean = ((a xor r) and (b xor r)) < 0

private fun subtractionOverflows(
    a: Long,
    b: Long,
    r: Long,
): Boolean = ((a xor b) and (a xor r)) < 0

private fun productOverflows(
    a: Long,
    b: Long,
    r: Long,
): Boolean {
    // (two integers of 32 bits never do)
    if (((a + 0x80000000L) or (b + 0x80000000L)) ushr 32 == 0L) return false
    return (a == -1L && b == Long.MIN_VALUE) || (a != 0L && r / a != b)
}

internal fun holds(
    kind: Int,
    a: Int,
    b: Int,
): Boolean =
    when (kind) {
        Op.C_LT -> a < b
        Op.C_GT -> a > b
        Op.C_LE -> a <= b
        Op.C_GE -> a >= b
        Op.C_EQ -> a == b
        else -> a != b
    }
