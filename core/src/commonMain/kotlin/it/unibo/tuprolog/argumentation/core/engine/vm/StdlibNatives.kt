package it.unibo.tuprolog.argumentation.core.engine.vm

import it.unibo.tuprolog.argumentation.core.engine.stdlib.DefaultBuiltins
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.ATOM
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.BIG
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.FLT
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.FUN
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.INT
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.LIST
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.REF
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.STR
import it.unibo.tuprolog.core.TermComparator
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive
import org.gciatto.kt.math.BigInteger

/**
 * A primitive of the standard library run on the words of the machine: [FAILED][Machine.FAILED],
 * [CONTINUE][Machine.CONTINUE], or [NOT_NATIVE][StdlibNatives.NOT_NATIVE] for the cases it leaves to the primitive of
 * the library, as it is: every error, and what the machine has no quick answer for. It reads the registers, and leaves
 * them and the heap's bindings untouched when it answers [NOT_NATIVE][StdlibNatives.NOT_NATIVE].
 */
internal fun interface StdlibNative {
    fun run(m: Machine): Int
}

/**
 * The primitives of the standard library that arg2p relies on, whose arguments would otherwise be read back as terms
 * (a cost that grows with the terms, however little the primitive looks at them). They stand in for the primitive of
 * [DefaultBuiltins] alone, found as any primitive is: a theory or a library defining the predicate keeps it.
 */
internal object StdlibNatives {
    const val NOT_NATIVE = -1

    /** Two terms the standard order cannot tell here: a variable is met (the library orders them by name) */
    const val UNORDERED = Int.MIN_VALUE

    private const val FAILED = Machine.FAILED
    private const val CONTINUE = Machine.CONTINUE

    private fun answer(holds: Boolean): Int = if (holds) CONTINUE else FAILED

    private fun typeTest(test: (Int) -> Boolean) = StdlibNative { m -> answer(test(tag(m.deref(m.regs[0])))) }

    private fun ordering(holds: (Int) -> Boolean) =
        StdlibNative { m ->
            val order = order(m, m.regs[0], m.regs[1])
            if (order == UNORDERED) NOT_NATIVE else answer(holds(order))
        }

    private val table: Map<Signature, StdlibNative> =
        mapOf(
            Signature("atom", 1) to typeTest { it == ATOM },
            Signature("atomic", 1) to typeTest { it == ATOM || it == INT || it == BIG || it == FLT },
            Signature("callable", 1) to typeTest { it == ATOM || it == STR || it == LIST },
            Signature("compound", 1) to typeTest { it == STR || it == LIST },
            Signature("float", 1) to typeTest { it == FLT },
            Signature("integer", 1) to typeTest { it == INT || it == BIG },
            Signature("number", 1) to typeTest { it == INT || it == BIG || it == FLT },
            Signature("ground", 1) to StdlibNative { m -> answer(ground(m, m.regs[0])) },
            Signature("==", 2) to StdlibNative { m -> answer(identical(m, m.regs[0], m.regs[1])) },
            Signature("\\==", 2) to StdlibNative { m -> answer(!identical(m, m.regs[0], m.regs[1])) },
            Signature("\\=", 2) to StdlibNative { m -> answer(!m.unifiable(m.regs[0], m.regs[1])) },
            Signature("@<", 2) to ordering { it < 0 },
            Signature("@=<", 2) to ordering { it <= 0 },
            Signature("@>", 2) to ordering { it > 0 },
            Signature("@>=", 2) to ordering { it >= 0 },
            Signature("functor", 3) to StdlibNative(::functor),
            Signature("arg", 3) to StdlibNative(::arg),
            Signature("=..", 2) to StdlibNative(::univ),
            Signature("copy_term", 2) to StdlibNative { m -> answer(m.unify(m.copyOnHeap(m.regs[0]), m.regs[1])) },
            Signature("reverse", 2) to StdlibNative(::reverse),
            Signature("atom_length", 2) to StdlibNative(::atomLength),
            Signature("atom_concat", 3) to StdlibNative(::atomConcat),
        )

    /** The native of [signature], if [primitive] is the one of the standard library */
    fun of(
        signature: Signature,
        primitive: Primitive?,
    ): StdlibNative? {
        val native = table[signature] ?: return null
        return if (primitive != null && primitive === DefaultBuiltins.primitives[signature]) native else null
    }

    // ---- terms

    /** Whether no variable is in the term [w] */
    fun ground(
        m: Machine,
        w: Int,
    ): Boolean {
        var d = m.deref(w)
        while (true) {
            when (tag(d)) {
                REF -> return false
                STR -> {
                    val a = payload(d)
                    val n = m.program.arity(m.heap[a])
                    for (i in 1 until n) if (!ground(m, m.heap[a + i])) return false
                    d = m.deref(m.heap[a + n])
                }
                LIST -> {
                    val a = payload(d)
                    if (!ground(m, m.heap[a])) return false
                    d = m.deref(m.heap[a + 1])
                }
                else -> return true
            }
        }
    }

    /** Whether the terms are identical, as `==`: the same variables, not bound */
    fun identical(
        m: Machine,
        x0: Int,
        y0: Int,
    ): Boolean {
        var x = m.deref(x0)
        var y = m.deref(y0)
        while (true) {
            if (x == y) return true
            val t = tag(x)
            if (t != tag(y)) return false
            when (t) {
                REF -> return m.sameVariable(payload(x), payload(y))
                STR -> {
                    val a = payload(x)
                    val b = payload(y)
                    val f = m.heap[a]
                    if (f != m.heap[b]) return false
                    val n = m.program.arity(f)
                    for (i in 1 until n) if (!identical(m, m.heap[a + i], m.heap[b + i])) return false
                    x = m.deref(m.heap[a + n])
                    y = m.deref(m.heap[b + n])
                }
                LIST -> {
                    val a = payload(x)
                    val b = payload(y)
                    if (!identical(m, m.heap[a], m.heap[b])) return false
                    x = m.deref(m.heap[a + 1])
                    y = m.deref(m.heap[b + 1])
                }
                else -> return false
            }
        }
    }

    /** A hash of the term, the same for those [identical]: all the variables count alike, two cells may be the same one */
    fun hash(
        m: Machine,
        w0: Int,
        seed: Int,
    ): Int {
        var hash = seed
        var w = m.deref(w0)
        while (true) {
            when (tag(w)) {
                REF -> return 31 * hash
                STR -> {
                    val a = payload(w)
                    val f = m.heap[a]
                    hash = 31 * hash + f
                    val n = m.program.arity(f)
                    for (i in 1 until n) hash = hash(m, m.heap[a + i], hash)
                    w = m.deref(m.heap[a + n])
                }
                LIST -> {
                    val a = payload(w)
                    hash = hash(m, m.heap[a], 31 * hash + 1)
                    w = m.deref(m.heap[a + 1])
                }
                else -> return 31 * hash + w
            }
        }
    }

    // The ranks of the standard order of tuProlog: reals before integers, atoms, compound terms
    private fun rank(t: Int): Int =
        when (t) {
            FLT -> 1
            INT, BIG -> 2
            ATOM -> 3
            else -> 4
        }

    private fun integerOrder(
        m: Machine,
        x: Int,
        y: Int,
    ): Int {
        val huge = (tag(x) == BIG && payload(x) < 0) || (tag(y) == BIG && payload(y) < 0)
        if (!huge) return longOf(m, x).compareTo(longOf(m, y))
        return bigOf(m, x).compareTo(bigOf(m, y))
    }

    private fun bigOf(
        m: Machine,
        w: Int,
    ): BigInteger = if (tag(w) == INT) BigInteger.of(payload(w).toLong()) else m.program.bigInteger(payload(w))

    private fun longOf(
        m: Machine,
        w: Int,
    ): Long = if (tag(w) == INT) payload(w).toLong() else m.program.bigInteger(payload(w)).toLong()

    /**
     * The terms in the standard order of tuProlog (see [TermComparator.DefaultComparator]): negative, zero, positive,
     * or [UNORDERED] when it depends on a variable
     */
    fun order(
        m: Machine,
        x0: Int,
        y0: Int,
    ): Int {
        var x = m.deref(x0)
        var y = m.deref(y0)
        val program = m.program
        while (true) {
            if (x == y) return 0
            val tx = tag(x)
            val ty = tag(y)
            if (tx == REF || ty == REF) return UNORDERED
            val rx = rank(tx)
            val ry = rank(ty)
            if (rx != ry) return rx - ry
            when (rx) {
                1 -> return TermComparator.RealComparator.compare(program.floats[payload(x)], program.floats[payload(y)])
                2 -> return integerOrder(m, x, y)
                3 -> return m.atomOrder(payload(x), payload(y))
                else -> {
                    val a = payload(x)
                    val b = payload(y)
                    val fx = if (tx == LIST) 0 else m.heap[a]
                    val fy = if (ty == LIST) 0 else m.heap[b]
                    val nx = if (tx == LIST) 2 else program.arity(fx)
                    val ny = if (ty == LIST) 2 else program.arity(fy)
                    if (nx != ny) return nx - ny
                    if (fx != fy) {
                        val nameX = if (tx == LIST) "." else program.functorName(fx)
                        val nameY = if (ty == LIST) "." else program.functorName(fy)
                        val names = nameX.compareTo(nameY)
                        if (names != 0) return names
                    }
                    val first = if (tx == LIST) 0 else 1
                    val second = if (ty == LIST) 0 else 1
                    for (i in 0 until nx - 1) {
                        val argument = order(m, m.heap[a + first + i], m.heap[b + second + i])
                        if (argument != 0) return argument
                    }
                    x = m.deref(m.heap[a + first + nx - 1])
                    y = m.deref(m.heap[b + second + ny - 1])
                }
            }
        }
    }

    // ---- primitives

    private fun functor(m: Machine): Int {
        val term = m.deref(m.regs[0])
        val arity = m.deref(m.regs[2])
        val t = tag(term)
        if (t == REF) {
            val name = m.deref(m.regs[1])
            if (tag(arity) != INT || payload(arity) < 0) return NOT_NATIVE
            if (payload(arity) == 0) {
                return when (tag(name)) {
                    ATOM, INT, FLT, BIG -> answer(m.unify(term, name))
                    else -> NOT_NATIVE
                }
            }
            if (tag(name) != ATOM) return NOT_NATIVE
            if (name == m.program.dotWord) {
                if (payload(arity) != 2) return NOT_NATIVE
                val cell = m.alloc(0)
                m.pushVoids(2)
                return answer(m.unify(term, word(LIST, cell)))
            }
            val n = payload(arity)
            val h = m.alloc(1)
            m.heap[h] = word(FUN, m.program.functor(payload(name), n))
            m.pushVoids(n)
            return answer(m.unify(term, word(STR, h)))
        }
        // an arity that is not one is left to the library, which raises its error
        if (tag(arity) != REF && (tag(arity) != INT || payload(arity) < 0)) return NOT_NATIVE
        val name: Int
        val n: Int
        when (t) {
            STR -> {
                val f = m.heap[payload(term)]
                name = word(ATOM, m.program.functorNames[payload(f)])
                n = m.program.arity(f)
            }
            LIST -> {
                name = m.program.dotWord
                n = 2
            }
            ATOM -> {
                name = term
                n = 0
            }
            else -> {
                name = term
                n = 0
            }
        }
        return answer(m.unify(m.regs[1], name) && m.unify(arity, m.program.intWord(n.toLong())))
    }

    private fun arg(m: Machine): Int {
        val index = m.deref(m.regs[0])
        val term = m.deref(m.regs[1])
        if (tag(index) != INT || payload(index) < 0) return NOT_NATIVE
        val i = payload(index)
        return when (tag(term)) {
            STR -> {
                val a = payload(term)
                if (i < 1 || i > m.program.arity(m.heap[a])) FAILED else answer(m.unify(m.regs[2], m.heap[a + i]))
            }
            LIST -> {
                if (i < 1 || i > 2) FAILED else answer(m.unify(m.regs[2], m.heap[payload(term) + i - 1]))
            }
            else -> NOT_NATIVE
        }
    }

    private fun univ(m: Machine): Int {
        val term = m.deref(m.regs[0])
        val list = m.deref(m.regs[1])
        val listTag = tag(list)
        return when (tag(term)) {
            REF -> compose(m, term, list)
            STR, LIST, ATOM -> {
                if (listTag != REF && listTag != LIST && list != m.program.emptyList) {
                    NOT_NATIVE
                } else {
                    answer(m.unify(list, decomposed(m, term)))
                }
            }
            else -> NOT_NATIVE
        }
    }

    private fun decomposed(
        m: Machine,
        term: Int,
    ): Int {
        val program = m.program
        val items: IntArray
        when (tag(term)) {
            STR -> {
                val a = payload(term)
                val f = m.heap[a]
                val n = program.arity(f)
                items = IntArray(n + 1)
                items[0] = program.atomWord(program.functorName(f))
                for (i in 1..n) items[i] = m.heap[a + i]
            }
            LIST -> {
                val a = payload(term)
                items = intArrayOf(program.atomWord("."), m.heap[a], m.heap[a + 1])
            }
            else -> items = intArrayOf(term)
        }
        return m.list(items, program.emptyList)
    }

    // Term =.. [Name | Arguments], with Term a variable: a proper list, its head an atom
    private fun compose(
        m: Machine,
        term: Int,
        list: Int,
    ): Int {
        if (tag(list) != LIST) return NOT_NATIVE
        val items = properItems(m, list) ?: return NOT_NATIVE
        val head = m.deref(items[0])
        if (tag(head) != ATOM) return NOT_NATIVE
        val name = m.program.atoms[payload(head)]
        if (items.size == 1) return answer(m.unify(term, head))
        if (name == ".") return NOT_NATIVE
        val h = m.alloc(items.size)
        m.heap[h] = m.program.functorWord(name, items.size - 1)
        for (i in 1 until items.size) m.heap[h + i] = items[i]
        return answer(m.unify(term, word(STR, h)))
    }

    // The items of the proper list [list]; null otherwise
    private fun properItems(
        m: Machine,
        list: Int,
    ): IntArray? {
        var items = IntArray(8)
        var n = 0
        var w = m.deref(list)
        while (tag(w) == LIST) {
            if (n == items.size) items = items.copyOf(n * 2)
            items[n++] = m.heap[payload(w)]
            w = m.deref(m.heap[payload(w) + 1])
        }
        return if (w == m.program.emptyList) items.copyOf(n) else null
    }

    private fun reverse(m: Machine): Int {
        val other = m.deref(m.regs[1])
        if (tag(other) != REF) return NOT_NATIVE
        var reversed = m.program.emptyList
        var w = m.deref(m.regs[0])
        while (tag(w) == LIST) {
            val item = m.heap[payload(w)]
            val cell = m.alloc(2)
            m.heap[cell] = item
            m.heap[cell + 1] = reversed
            reversed = word(LIST, cell)
            w = m.deref(m.heap[payload(w) + 1])
        }
        if (w != m.program.emptyList) return NOT_NATIVE
        return answer(m.unify(reversed, other))
    }

    private fun atomLength(m: Machine): Int {
        val atom = m.deref(m.regs[0])
        val size = m.deref(m.regs[1])
        if (tag(atom) != ATOM) return NOT_NATIVE
        if (tag(size) != REF && (tag(size) != INT || payload(size) < 0)) return NOT_NATIVE
        return answer(
            m.unify(
                size,
                m.program.intWord(
                    m.program.atoms[payload(atom)]
                        .length
                        .toLong(),
                ),
            ),
        )
    }

    private fun atomConcat(m: Machine): Int {
        val left = m.deref(m.regs[0])
        val right = m.deref(m.regs[1])
        val whole = m.deref(m.regs[2])
        val program = m.program

        fun text(w: Int) = program.atoms[payload(w)]
        return when {
            tag(left) == ATOM && tag(right) == ATOM && (tag(whole) == REF || tag(whole) == ATOM) ->
                answer(m.unify(whole, program.atomWord(text(left) + text(right))))
            tag(left) == REF && tag(right) == ATOM && tag(whole) == ATOM ->
                if (text(whole).endsWith(text(right))) {
                    answer(m.unify(left, program.atomWord(text(whole).removeSuffix(text(right)))))
                } else {
                    FAILED
                }
            tag(right) == REF && tag(left) == ATOM && tag(whole) == ATOM ->
                if (text(whole).startsWith(text(left))) {
                    answer(m.unify(right, program.atomWord(text(whole).removePrefix(text(left)))))
                } else {
                    FAILED
                }
            else -> NOT_NATIVE
        }
    }
}
