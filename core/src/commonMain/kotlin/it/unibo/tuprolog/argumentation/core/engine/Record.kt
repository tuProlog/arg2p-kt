package it.unibo.tuprolog.argumentation.core.engine

import it.unibo.tuprolog.argumentation.core.engine.vm.Program
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.ATOM
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.BIG
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.FLT
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.INT
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.LIST
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.REF
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.STR
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.payload
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.ref
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.tag
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.word
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Cons
import it.unibo.tuprolog.core.Fact
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Real
import it.unibo.tuprolog.core.Rule
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.core.List as LogicList

/**
 * A term kept outside the heap of the machine, as its words (a copy with relative addresses, its variables fresh
 * each time it is put back): what the stores of arg2p hold, cheap to take from the heap ([NativeCall.record]) and
 * to put back ([NativeCall.recall]), read as a term only when something asks for one. A record made from a term
 * ([of]) holds the term and no words until a store encodes it.
 */
class Record internal constructor(
    /** The program whose words these are; null for a record holding a term alone */
    internal val program: Program?,
    internal val words: IntArray,
    /** The root word, relative to [words] */
    internal val root: Int,
    source: Term?,
    /** The functor of the root in place of the one in [words] (see [named]), 0 for that one */
    internal val functor: Int = 0,
) {
    /** The identifier of the record, as the identifiers of a context know it (see `context_id/2`); -1 for none */
    internal var identifier: Int = -1

    /** The term, decoded once */
    val term: Term by lazy(LazyThreadSafetyMode.NONE) { source ?: decode(root, HashMap()) }

    /** The clause: `Head :- Body` read as a rule, anything else as a fact */
    val clause: Clause by lazy(LazyThreadSafetyMode.NONE) {
        val t = term
        if (t is Struct && t.functor == ":-" && t.arity == 2) Rule.of(t[0].castToStruct(), t[1]) else Fact.of(t.castToStruct())
    }

    /** Whether the record is `Head :- Body` */
    val isRule: Boolean =
        if (program == null) {
            term.let { it is Struct && it.functor == ":-" && it.arity == 2 }
        } else {
            tag(root) == STR && words[payload(root)] == program.ruleFunctor
        }

    /** The head: the term itself for a fact (relative word; only with a program) */
    internal val head: Int get() = if (isRule) words[payload(root) + 1] else root

    /** The body: `true` for a fact (relative word; only with a program) */
    internal val body: Int get() = if (isRule) words[payload(root) + 2] else program!!.trueWord

    /** The same words under the functor [functor], of the same arity: the fact `other(A)` of the fact `name(A)`, not copied */
    internal fun named(functor: Int): Record = Record(program, words, root, null, functor)

    /**
     * The record as a key: the same for two terms up to the names of their variables, however they are laid out (in the
     * words of one program)
     */
    internal val key: RecordKey by lazy(LazyThreadSafetyMode.NONE) {
        val flat = Flattened(program!!, words)
        flat.add(root)
        RecordKey(flat.items.copyOf(flat.size))
    }

    private fun decode(
        w: Int,
        variables: HashMap<Int, Var>,
    ): Term {
        val program = program!!
        return when (tag(w)) {
            REF -> variables.getOrPut(payload(w)) { Var.of("V") }
            ATOM -> Atom.of(program.atoms[payload(w)])
            INT -> Integer.of(payload(w))
            BIG -> Integer.of(program.bigs[payload(w)])
            FLT -> program.floats[payload(w)]
            STR -> {
                val a = payload(w)
                val f = if (functor != 0 && w == root) functor else words[a]
                val n = program.arity(f)
                Struct.of(program.functorName(f), List(n) { decode(words[a + 1 + it], variables) })
            }
            else -> {
                val heads = ArrayList<Term>()
                var a = payload(w)
                while (true) {
                    heads.add(decode(words[a], variables))
                    val t = words[a + 1]
                    if (tag(t) == LIST) a = payload(t) else return LogicList.from(heads, decode(t, variables))
                }
                @Suppress("UNREACHABLE_CODE")
                throw IllegalStateException()
            }
        }
    }

    companion object {
        /** [term] as a record holding the term, encoded by the store that keeps it */
        fun of(term: Term): Record = Record(null, IntArray(0), 0, term)

        /** [term] as the words of [program] */
        internal fun of(
            term: Term,
            program: Program,
        ): Record {
            val encoder = Encoder(program)
            val root = encoder.encode(term, HashMap())
            return Record(program, encoder.words.copyOf(encoder.top), root, null)
        }

        /** [record] in the words of [program], itself if it is there already */
        internal fun encoded(
            record: Record,
            program: Program,
        ): Record = if (record.program === program) record else of(record.term, program)
    }

    private class Encoder(
        private val program: Program,
    ) {
        var words = IntArray(16)
        var top = 0

        private fun alloc(cells: Int): Int {
            if (top + cells > words.size) words = words.copyOf(maxOf(words.size * 2, top + cells))
            val start = top
            top += cells
            return start
        }

        fun encode(
            term: Term,
            variables: HashMap<Var, Int>,
        ): Int =
            when (term) {
                is Var ->
                    ref(
                        variables.getOrPut(term) {
                            val a = alloc(1)
                            words[a] = ref(a)
                            a
                        },
                    )
                is Integer -> program.intWord(term.value.toLong())
                is Real -> program.floatWord(term)
                is Atom -> program.atomWord(term.value)
                is Cons -> {
                    val first = alloc(2)
                    var s = first
                    var list: Term = term
                    while (true) {
                        val cons = list as Cons
                        val head = encode(cons.head, variables)
                        words[s] = head
                        val tail = cons.tail
                        if (tail is Cons) {
                            val s2 = alloc(2)
                            words[s + 1] = word(LIST, s2)
                            s = s2
                            list = tail
                        } else {
                            val t = encode(tail, variables)
                            words[s + 1] = t
                            break
                        }
                    }
                    word(LIST, first)
                }
                is Struct -> {
                    val n = term.arity
                    val s = alloc(n + 1)
                    words[s] = program.functorWord(term.functor, n)
                    for (i in 0 until n) {
                        val arg = encode(term[i], variables)
                        words[s + 1 + i] = arg
                    }
                    word(STR, s)
                }
                else -> throw IllegalArgumentException("not a term of the machine: $term")
            }
    }
}

/** A term up to the names of its variables: see [Record.key] */
internal class RecordKey(
    private val flat: IntArray,
) {
    private val hash = flat.contentHashCode()

    override fun hashCode(): Int = hash

    override fun equals(other: Any?): Boolean = other is RecordKey && other.flat.contentEquals(flat)
}

// A term in preorder: the words of its symbols, a variable as the order it first came in
private class Flattened(
    private val program: Program,
    private val words: IntArray,
) {
    var items = IntArray(32)
    var size = 0
    private var variables: HashMap<Int, Int>? = null

    private fun push(w: Int) {
        if (size == items.size) items = items.copyOf(size * 2)
        items[size++] = w
    }

    fun add(w0: Int) {
        var w = w0
        while (true) {
            when (tag(w)) {
                REF -> {
                    val known = variables ?: HashMap<Int, Int>().also { variables = it }
                    push(ref(known.getOrPut(payload(w)) { known.size }))
                    return
                }
                STR -> {
                    val a = payload(w)
                    val f = words[a]
                    push(f)
                    val n = program.arity(f)
                    for (i in 1 until n) add(words[a + i])
                    w = words[a + n]
                }
                LIST -> {
                    val a = payload(w)
                    push(Program.LIST_KEY)
                    add(words[a])
                    w = words[a + 1]
                }
                else -> {
                    push(w)
                    return
                }
            }
        }
    }
}

/** The goals of a body wrapped as a native primitive asks (see the lookups of the stores) */
fun interface GoalWrapper {
    fun wrap(
        call: NativeCall,
        goal: Int,
    ): Int
}
