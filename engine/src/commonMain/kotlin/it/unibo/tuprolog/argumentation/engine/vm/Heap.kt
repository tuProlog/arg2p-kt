package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.vm.program.Numbers
import it.unibo.tuprolog.argumentation.engine.vm.program.Program
import it.unibo.tuprolog.argumentation.engine.vm.program.TermReader
import it.unibo.tuprolog.argumentation.engine.vm.program.TermWriter
import it.unibo.tuprolog.argumentation.engine.words.Words
import it.unibo.tuprolog.argumentation.engine.words.Words.BIG
import it.unibo.tuprolog.argumentation.engine.words.Words.FLT
import it.unibo.tuprolog.argumentation.engine.words.Words.INT
import it.unibo.tuprolog.argumentation.engine.words.Words.LIST
import it.unibo.tuprolog.argumentation.engine.words.Words.REF
import it.unibo.tuprolog.argumentation.engine.words.Words.STR
import it.unibo.tuprolog.argumentation.engine.words.payload
import it.unibo.tuprolog.argumentation.engine.words.ref
import it.unibo.tuprolog.argumentation.engine.words.tag
import it.unibo.tuprolog.argumentation.engine.words.word
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import kotlin.jvm.JvmField

/**
 * The terms of a machine: the cells of the heap (words, see [Words]), allocated at the top, and the bindings of their
 * variables. A binding of a cell below the [barrier] (the top of the heap at the latest choice point) is trailed, so that
 * [undoTrail] back to a mark leaves the cells as they were at it; nothing else writes the cell of a variable bound. Here too
 * the unification, the words of numbers, and the terms of tuProlog written on the heap and read from it.
 *
 * The arrays grow by copy: a reference to them is good until the next allocation.
 */
internal class Heap(
    private val program: Program,
    @JvmField var cells: IntArray,
    // What a cell holding an unbound variable is called when the term is read: 0 nothing, a name atom and one for a
    // variable the code made (fresh as read), minus an index and one for the variable object the cell was made for
    // (the same object as read again: a stored term read twice gives the same variables)
    @JvmField var varInfo: IntArray,
    @JvmField var trail: IntArray,
) : HeapCollector.Roots {
    @JvmField var top = 0

    @JvmField var trailTop = 0

    // The cells below this address are trailed when bound
    @JvmField var barrier = 0

    // The cells of the variables of the query (the first ones), and when each was bound (the bindings of a solution are
    // reported in the order they were made)
    @JvmField var queryCells = 0

    @JvmField var queryBound = IntArray(16)

    @JvmField var bindings = 0

    // The stack of the unification of compounds
    private var pdl = IntArray(1 shl 10)

    private val varObjects = ArrayList<Var>()

    // (the trail: the cells bound since a choice point, kept for backtracking to unbind them)
    override fun markRoots(c: HeapCollector) {
        for (i in 0 until trailTop) c.markCell(trail[i])
    }

    override fun relocateRoots(c: HeapCollector) {
        for (i in 0 until trailTop) trail[i] = c.newAddress(trail[i])
    }

    /** Empty: a query starts from nothing */
    fun reset() {
        varObjects.clear()
        top = 0
        barrier = 0
        trailTop = 0
    }

    // ---- cells

    /** [n] cells reserved on top: the first of them */
    fun alloc(n: Int): Int {
        val h = top
        if (h + n > cells.size) grow(h + n)
        top = h + n
        return h
    }

    fun grow(n: Int) {
        val size = maxOf(cells.size * 2, n)
        cells = cells.copyOf(size)
        varInfo = varInfo.copyOf(size)
    }

    // A cell holding [w] allocated at the top (`cells[alloc(1)] = w` would store into the array read before alloc grew it)
    fun pushCell(w: Int) {
        if (top == cells.size) grow(top + 1)
        cells[top++] = w
    }

    /** A fresh unbound variable, called by [info] when read (see varInfo) */
    fun newVariable(info: Int = 0): Int {
        val a = alloc(1)
        cells[a] = ref(a)
        varInfo[a] = info
        return a
    }

    /** [n] fresh unbound variables on top */
    fun pushVoids(n: Int) {
        val h = alloc(n)
        for (i in h until h + n) {
            cells[i] = ref(i)
            varInfo[i] = 0
        }
    }

    // ---- dereferencing

    // (most words are no reference: that test alone here, small enough for the JIT of the JVM to inline it at every site, 35
    // bytes of bytecode; the chain of references walked by a call)
    fun deref(w0: Int): Int = if (tag(w0) != REF) w0 else derefChain(w0)

    private fun derefChain(w0: Int): Int = dereferenced(w0)

    // [deref] in place: the JIT of the JVM judges most of the sites calling it cold, and leaves them calls
    @Suppress("NOTHING_TO_INLINE")
    inline fun dereferenced(w0: Int): Int {
        var w = w0
        while (tag(w) == REF) {
            val c = cells[payload(w)]
            if (c == w) return w
            w = c
        }
        return w
    }

    // ---- binding and unification

    fun bind(
        address: Int,
        w: Int,
    ) {
        cells[address] = w
        if (address < queryCells) queryBound[address] = ++bindings
        if (address < barrier) {
            if (trailTop == trail.size) trail = trail.copyOf(trail.size * 2)
            trail[trailTop++] = address
        }
    }

    /** The bindings made since the trail was at [mark] undone */
    fun undoTrail(mark: Int) {
        while (trailTop > mark) {
            val a = trail[--trailTop]
            cells[a] = ref(a)
        }
    }

    /** What [block] does, then undone: every binding trailed, the trail and the top back as they were */
    inline fun tentatively(block: () -> Boolean): Boolean {
        val savedTop = top
        val savedBarrier = barrier
        val mark = trailTop
        barrier = Int.MAX_VALUE
        val holds = block()
        undoTrail(mark)
        barrier = savedBarrier
        top = savedTop
        return holds
    }

    fun unify(
        x0: Int,
        y0: Int,
    ): Boolean {
        // The plain cases first, without the stack: the same word, a variable on either side, two constants
        val x1 = dereferenced(x0)
        val y1 = dereferenced(y0)
        if (x1 == y1) return true
        val tx1 = tag(x1)
        val ty1 = tag(y1)
        if (tx1 == REF) {
            if (ty1 == REF) bindVariables(payload(x1), payload(y1)) else bind(payload(x1), y1)
            return true
        }
        if (ty1 == REF) {
            bind(payload(y1), x1)
            return true
        }
        if (tx1 != ty1 || (tx1 != STR && tx1 != LIST)) return false
        return unifyCompounds(x1, y1)
    }

    // Two compound terms of the same tag, argument by argument
    private fun unifyCompounds(
        x1: Int,
        y1: Int,
    ): Boolean {
        var sp = 0
        pdl[sp++] = x1
        pdl[sp++] = y1
        while (sp > 0) {
            val y = deref(pdl[--sp])
            val x = deref(pdl[--sp])
            if (x == y) continue
            val tx = tag(x)
            val ty = tag(y)
            if (tx == REF) {
                if (ty == REF) bindVariables(payload(x), payload(y)) else bind(payload(x), y)
                continue
            }
            if (ty == REF) {
                bind(payload(y), x)
                continue
            }
            if (tx != ty) return false
            when (tx) {
                STR -> {
                    val ax = payload(x)
                    val ay = payload(y)
                    if (cells[ax] != cells[ay]) return false
                    val n = program.arity(cells[ax])
                    if (sp + 2 * n > pdl.size) pdl = pdl.copyOf(maxOf(pdl.size * 2, sp + 2 * n))
                    for (i in 1..n) {
                        pdl[sp++] = cells[ax + i]
                        pdl[sp++] = cells[ay + i]
                    }
                }
                LIST -> {
                    val ax = payload(x)
                    val ay = payload(y)
                    if (sp + 4 > pdl.size) pdl = pdl.copyOf(pdl.size * 2)
                    pdl[sp++] = cells[ax + 1]
                    pdl[sp++] = cells[ay + 1]
                    pdl[sp++] = cells[ax]
                    pdl[sp++] = cells[ay]
                }
                else -> return false
            }
        }
        return true
    }

    // Two variables met: the first bound to the second (X = Y binds X, as Arg2pTest expects; either way is safe here, both
    // cells being on the heap and the older one trailed)
    private fun bindVariables(
        a: Int,
        b: Int,
    ) {
        bind(a, ref(b))
    }

    /** A constant against a word: bound to it if a variable, the same word otherwise */
    fun unifyConstant(
        constant: Int,
        w: Int,
    ): Boolean {
        val d = dereferenced(w)
        if (tag(d) == REF) {
            bind(payload(d), constant)
            return true
        }
        return d == constant
    }

    /** Whether the two unbound cells hold one variable: those read as the same variable of a term given to the machine */
    fun sameVariable(
        a: Int,
        b: Int,
    ): Boolean {
        if (a == b) return true
        val x = varInfo[a]
        val y = varInfo[b]
        return x < 0 && y < 0 && varObjects[-x - 1] == varObjects[-y - 1]
    }

    // ---- numbers

    /** The word of the integer [value]: a small one as it is, another a compound on the heap (see [Numbers]) */
    fun intWord(value: Long): Int = if (Words.fitsInt(value)) word(INT, value.toInt()) else largeIntWord(value)

    // (apart, for intWord to stay small enough for the JIT to inline: Numbers.write is inline, and large)
    private fun largeIntWord(value: Long): Int = Numbers.write(program, value, { alloc(it) }, { a, w -> cells[a] = w })

    /** The word of the sum of two small integers */
    fun sumWord(
        a: Int,
        b: Int,
    ): Int {
        val sum = a.toLong() + b
        return if (Words.fitsInt(sum)) word(INT, sum.toInt()) else largeIntWord(sum)
    }

    /**
     * The tag a type test reads of the dereferenced word [w]: its own, but [BIG] or [FLT] for the compound of a number no word
     * holds (see [Numbers])
     */
    fun typeTag(w: Int): Int {
        val t = tag(w)
        if (t != STR) return t
        val n = program.numberTag(cells[payload(w)])
        return if (n == 0) STR else n
    }

    /** The integer of the integer compound [w] (see [Numbers]) */
    fun bigIntegerOfNumber(w: Int): org.gciatto.kt.math.BigInteger {
        val a = payload(w)
        val n = program.arity(cells[a])
        return Numbers.bigOf(payload(deref(cells[a + 1])), IntArray(n - 1) { payload(deref(cells[a + 2 + it])) })
    }

    // ---- terms in and out

    private val writer =
        object : TermWriter(program) {
            override fun alloc(cells: Int): Int = this@Heap.alloc(cells)

            override fun put(
                address: Int,
                w: Int,
            ) {
                cells[address] = w
            }

            override fun variable(term: Var): Int {
                varObjects.add(term)
                return newVariable(-varObjects.size)
            }
        }

    private val reader =
        object : TermReader(program) {
            override fun cell(address: Int): Int = cells[address]

            // The variable a cell holding an unbound one is read as (see varInfo)
            override fun variable(address: Int): Var {
                val info = varInfo[address]
                return when {
                    info > 0 -> Var.of(program.symbols.atoms[info - 1])
                    info < 0 -> varObjects[-info - 1]
                    else -> Var.of("_G$address")
                }
            }
        }

    /** [term] on the heap, its variables as cells (those of [variables], allocated as met) */
    fun fromTerm(
        term: Term,
        variables: HashMap<Var, Int>,
    ): Int = writer.write(term, variables)

    /** The term of the word [w0], its unbound cells as the variables of [names] */
    fun toTerm(
        w0: Int,
        names: HashMap<Int, Var>,
    ): Term = reader.read(w0, names)

    /** The variable a query gives the cell made for it */
    fun queryVariable(variable: Var): Int {
        varObjects.add(variable)
        return newVariable(-varObjects.size)
    }
}
