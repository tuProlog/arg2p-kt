package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.vm.NativeCall.Companion.CONTINUE
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall.Companion.FAIL
import it.unibo.tuprolog.argumentation.engine.words.Words
import it.unibo.tuprolog.argumentation.engine.words.Words.ATOM
import it.unibo.tuprolog.argumentation.engine.words.Words.BIG
import it.unibo.tuprolog.argumentation.engine.words.Words.FLT
import it.unibo.tuprolog.argumentation.engine.words.Words.INT
import it.unibo.tuprolog.argumentation.engine.words.Words.LIST
import it.unibo.tuprolog.argumentation.engine.words.Words.REF
import it.unibo.tuprolog.argumentation.engine.words.Words.STR
import it.unibo.tuprolog.argumentation.engine.words.payload
import it.unibo.tuprolog.argumentation.engine.words.tag
import it.unibo.tuprolog.argumentation.engine.words.word
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.stdlib.CommonBuiltins

/**
 * A primitive of the standard library run on the words of the machine: [NativeCall.FAIL],
 * [NativeCall.CONTINUE], or [NOT_NATIVE][StdlibNatives.NOT_NATIVE] for the cases it leaves to the primitive of
 * the library, as it is: every error, and what the machine has no quick answer for. It reads the registers, and leaves
 * them and the heap's bindings untouched when it answers [NOT_NATIVE][StdlibNatives.NOT_NATIVE].
 */
internal fun interface StdlibNative {
    fun run(m: Machine): Int
}

/**
 * The primitives of the standard library that arg2p relies on, whose arguments would otherwise be read back as terms
 * (a cost that grows with the terms, however little the primitive looks at them). They stand in for the primitive of
 * the standard library alone, found as any primitive is: a theory or a library defining the predicate keeps it.
 */
internal object StdlibNatives {
    const val NOT_NATIVE = -1

    private fun answer(holds: Boolean): Int = if (holds) CONTINUE else FAIL

    private fun typeTest(mask: Int) = StdlibNative { m -> answer((mask ushr tag(m.deref(m.regs[0]))) and 1 != 0) }

    private fun ordering(holds: (Int) -> Boolean) =
        StdlibNative { m ->
            val order = m.termOrder.order(m.regs[0], m.regs[1])
            if (order == TermOrder.UNORDERED) NOT_NATIVE else answer(holds(order))
        }

    private val table: Map<Signature, StdlibNative> =
        Words.TYPE_TESTS.entries.associate { (name, mask) -> Signature(name, 1) to typeTest(mask) } +
            mapOf(
                Signature("ground", 1) to StdlibNative { m -> answer(HeapTerms.ground(m, m.regs[0])) },
                Signature("==", 2) to StdlibNative { m -> answer(HeapTerms.identical(m, m.regs[0], m.regs[1])) },
                Signature("\\==", 2) to StdlibNative { m -> answer(!HeapTerms.identical(m, m.regs[0], m.regs[1])) },
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

    // The primitives of the standard library, read once
    private val standard: Map<Signature, Primitive> by lazy { CommonBuiltins.primitives }

    /** The native of [signature], if [primitive] is the one of the standard library */
    fun of(
        signature: Signature,
        primitive: Primitive?,
    ): StdlibNative? {
        val native = table[signature] ?: return null
        return if (primitive != null && primitive === standard[signature]) native else null
    }

    // ---- primitives

    private fun functor(m: Machine): Int {
        val term = m.deref(m.regs[0])
        val arity = m.deref(m.regs[2])
        if (tag(term) == REF && tag(arity) == INT && payload(arity) == 0) {
            val name = m.deref(m.regs[1])
            return when (tag(name)) {
                ATOM, INT, FLT, BIG -> answer(m.unify(term, name))
                else -> NOT_NATIVE
            }
        }
        return m.functorOnRegisters()
    }

    private fun arg(m: Machine): Int {
        val index = m.deref(m.regs[0])
        return if (tag(index) != INT || payload(index) < 0) NOT_NATIVE else m.argOnRegisters(payload(index))
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
                items[0] = program.atomWord(program.nameOfFunctorWord(f))
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
        val name = m.program.symbols.atoms[payload(head)]
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
                    m.program.symbols.atoms[payload(atom)]
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

        fun text(w: Int) = program.symbols.atoms[payload(w)]
        return when {
            tag(left) == ATOM && tag(right) == ATOM && (tag(whole) == REF || tag(whole) == ATOM) ->
                answer(m.unify(whole, program.atomWord(text(left) + text(right))))
            tag(left) == REF && tag(right) == ATOM && tag(whole) == ATOM ->
                if (text(whole).endsWith(text(right))) {
                    answer(m.unify(left, program.atomWord(text(whole).removeSuffix(text(right)))))
                } else {
                    FAIL
                }
            tag(right) == REF && tag(left) == ATOM && tag(whole) == ATOM ->
                if (text(whole).startsWith(text(left))) {
                    answer(m.unify(right, program.atomWord(text(whole).removePrefix(text(left)))))
                } else {
                    FAIL
                }
            else -> NOT_NATIVE
        }
    }
}
