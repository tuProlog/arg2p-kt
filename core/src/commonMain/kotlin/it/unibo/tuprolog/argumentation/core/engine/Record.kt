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
) {
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
            tag(root) == STR && words[payload(root)] == program.functorWord(":-", 2)
        }

    /** The head: the term itself for a fact (relative word; only with a program) */
    internal val head: Int get() = if (isRule) words[payload(root) + 1] else root

    /** The body: `true` for a fact (relative word; only with a program) */
    internal val body: Int get() = if (isRule) words[payload(root) + 2] else program!!.atomWord("true")

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
                val n = program.arity(words[a])
                Struct.of(program.functorName(words[a]), List(n) { decode(words[a + 1 + it], variables) })
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

/** The goals of a body wrapped as a native primitive asks (see [NativeCall.sourcedRecords]) */
fun interface GoalWrapper {
    fun wrap(
        call: NativeCall,
        goal: Int,
    ): Int
}
