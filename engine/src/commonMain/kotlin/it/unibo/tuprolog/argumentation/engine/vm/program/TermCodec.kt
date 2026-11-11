package it.unibo.tuprolog.argumentation.engine.vm.program

import it.unibo.tuprolog.argumentation.engine.store.RecordKey
import it.unibo.tuprolog.argumentation.engine.words.Words
import it.unibo.tuprolog.argumentation.engine.words.Words.ATOM
import it.unibo.tuprolog.argumentation.engine.words.Words.BIG
import it.unibo.tuprolog.argumentation.engine.words.Words.FLT
import it.unibo.tuprolog.argumentation.engine.words.Words.FUN
import it.unibo.tuprolog.argumentation.engine.words.Words.INT
import it.unibo.tuprolog.argumentation.engine.words.Words.LIST
import it.unibo.tuprolog.argumentation.engine.words.Words.REF
import it.unibo.tuprolog.argumentation.engine.words.Words.STR
import it.unibo.tuprolog.argumentation.engine.words.payload
import it.unibo.tuprolog.argumentation.engine.words.ref
import it.unibo.tuprolog.argumentation.engine.words.tag
import it.unibo.tuprolog.argumentation.engine.words.word
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Cons
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Real
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.core.List as LogicList

/** A term written as words of [program] where the subclass keeps them: on the heap of a machine, in a record */
internal abstract class TermWriter(
    private val program: Program,
) {
    /** Room for [cells] cells: the address of the first */
    protected abstract fun alloc(cells: Int): Int

    protected abstract fun put(
        address: Int,
        w: Int,
    )

    /** The address of a new cell for the variable [term] */
    protected abstract fun variable(term: Var): Int

    /** The word of [term], its variables as cells (those of [variables], allocated as met) */
    fun write(
        term: Term,
        variables: HashMap<Var, Int>,
    ): Int =
        when (term) {
            is Var -> ref(variables.getOrPut(term) { variable(term) })
            is Atom -> program.atomWord(term.value)
            is Cons -> {
                val first = alloc(2)
                var s = first
                var list: Term = term
                while (true) {
                    val cons = list as Cons
                    val head = write(cons.head, variables)
                    put(s, head)
                    val tail = cons.tail
                    if (tail is Cons) {
                        val s2 = alloc(2)
                        put(s + 1, word(LIST, s2))
                        s = s2
                        list = tail
                    } else {
                        val t = write(tail, variables)
                        put(s + 1, t)
                        break
                    }
                }
                word(LIST, first)
            }
            is Struct -> {
                val n = term.arity
                val s = alloc(n + 1)
                put(s, word(FUN, program.functor(term.functor, n)))
                for (i in 0 until n) {
                    val arg = write(term[i], variables)
                    put(s + 1 + i, arg)
                }
                word(STR, s)
            }
            is Integer ->
                if (Numbers.needsBlock(
                        term,
                    )
                ) {
                    Numbers.write(program, term.value, { alloc(it) }, { a, w -> put(a, w) })
                } else {
                    program.constantWord(term)
                }
            is Real -> Numbers.write(program, term, { alloc(it) }, { a, w -> put(a, w) })
            else -> program.constantWord(term)
        }
}

/** A term read from words of [program] where the subclass keeps them: on the heap of a machine, in a record */
internal abstract class TermReader(
    private val program: Program,
) {
    protected abstract fun cell(address: Int): Int

    /** The variable the unbound cell at [address] is read as */
    protected abstract fun variable(address: Int): Var

    /** The functor word of the structure [w], at [address] */
    protected open fun functorOf(
        w: Int,
        address: Int,
    ): Int = cell(address)

    private fun deref(w0: Int): Int {
        var w = w0
        while (tag(w) == REF) {
            val c = cell(payload(w))
            if (c == w) return w
            w = c
        }
        return w
    }

    /** The term of the word [w0], its unbound cells as the variables of [names] */
    fun read(
        w0: Int,
        names: HashMap<Int, Var>,
    ): Term {
        val w = deref(w0)
        return when (tag(w)) {
            REF -> names.getOrPut(payload(w)) { variable(payload(w)) }
            ATOM -> program.atomTerm(payload(w))
            INT -> program.integerTerm(payload(w))
            STR -> {
                val a = payload(w)
                val f = functorOf(w, a)
                val n = program.arity(f)
                when (program.numberTag(f)) {
                    BIG -> Integer.of(Numbers.bigOf(payload(deref(cell(a + 1))), IntArray(n - 1) { payload(deref(cell(a + 2 + it))) }))
                    FLT ->
                        program.realOf(
                            payload(deref(cell(a + 1))),
                            payload(deref(cell(a + 2))),
                            IntArray(n - 2) {
                                payload(
                                    deref(
                                        cell(
                                            a + 3 + it,
                                        ),
                                    ),
                                )
                            },
                        )
                    else -> Struct.of(program.nameOfFunctorWord(f), List(n) { read(cell(a + 1 + it), names) })
                }
            }
            LIST -> {
                val heads = ArrayList<Term>()
                var a = payload(w)
                while (true) {
                    heads.add(read(cell(a), names))
                    val t = deref(cell(a + 1))
                    if (tag(t) == LIST) {
                        a = payload(t)
                    } else {
                        return LogicList.from(heads, read(t, names))
                    }
                }
                @Suppress("UNREACHABLE_CODE")
                throw IllegalStateException()
            }
            else -> throw IllegalStateException("not a term: $w")
        }
    }
}

/**
 * A term in preorder, as the key of a record ([RecordKey]): the words of its symbols, a variable as the order it first
 * came in. The same for two terms up to the names of their variables, on the heap or in a record, however they are laid
 * out. The key a call gives holds the buffer of this flattener, which the next call reuses: one to keep needs one of its own.
 */
internal class TermFlattener(
    private val program: Program,
    capacity: Int,
) {
    private var items = IntArray(capacity)
    private var size = 0

    // The variables met, by their addresses: in a table as they are few, none in most terms
    private var seen: IntArray? = null
    private var seenTop = 0
    private var seenMany: HashMap<Int, Int>? = null

    /** The key of the term [w] of [words] */
    fun keyOf(
        words: IntArray,
        w: Int,
    ): RecordKey {
        size = 0
        seenTop = 0
        seenMany = null
        add(words, w)
        return RecordKey(items, size)
    }

    private fun push(w: Int) {
        if (size == items.size) items = items.copyOf(maxOf(size * 2, 16))
        items[size++] = w
    }

    private fun orderOf(address: Int): Int {
        val many = seenMany
        if (many != null) return many.getOrPut(address) { many.size }
        val table = seen ?: IntArray(SEEN_IN_TABLE).also { seen = it }
        var i = 0
        while (i < seenTop && table[i] != address) i++
        if (i < seenTop) return i
        if (i < table.size) {
            table[seenTop++] = address
            return i
        }
        val all = HashMap<Int, Int>()
        for (k in 0 until seenTop) all[table[k]] = k
        all[address] = seenTop
        seenMany = all
        return seenTop
    }

    private fun add(
        words: IntArray,
        w0: Int,
    ) {
        var w = w0
        while (true) {
            when (tag(w)) {
                REF -> {
                    val c = words[payload(w)]
                    if (c != w) {
                        w = c
                    } else {
                        push(ref(orderOf(payload(w))))
                        return
                    }
                }
                STR -> {
                    val a = payload(w)
                    val f = words[a]
                    push(f)
                    val n = program.arity(f)
                    for (i in 1 until n) add(words, words[a + i])
                    w = words[a + n]
                }
                LIST -> {
                    val a = payload(w)
                    push(Words.LIST_KEY)
                    add(words, words[a])
                    w = words[a + 1]
                }
                else -> {
                    push(w)
                    return
                }
            }
        }
    }

    private companion object {
        const val SEEN_IN_TABLE = 64
    }
}
