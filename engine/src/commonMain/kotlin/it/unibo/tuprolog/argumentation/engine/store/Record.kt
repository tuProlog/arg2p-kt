package it.unibo.tuprolog.argumentation.engine.store

import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi
import it.unibo.tuprolog.argumentation.engine.vm.program.Program
import it.unibo.tuprolog.argumentation.engine.vm.program.TermFlattener
import it.unibo.tuprolog.argumentation.engine.vm.program.TermReader
import it.unibo.tuprolog.argumentation.engine.vm.program.TermWriter
import it.unibo.tuprolog.argumentation.engine.words.Words.ATOM
import it.unibo.tuprolog.argumentation.engine.words.Words.INT
import it.unibo.tuprolog.argumentation.engine.words.Words.LIST
import it.unibo.tuprolog.argumentation.engine.words.Words.REF
import it.unibo.tuprolog.argumentation.engine.words.Words.STR
import it.unibo.tuprolog.argumentation.engine.words.payload
import it.unibo.tuprolog.argumentation.engine.words.ref
import it.unibo.tuprolog.argumentation.engine.words.shifted
import it.unibo.tuprolog.argumentation.engine.words.tag
import it.unibo.tuprolog.argumentation.engine.words.word
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Fact
import it.unibo.tuprolog.core.Rule
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var

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
    private val source: Term?,
    /** The functor of the root in place of the one in [words] (see [named]), 0 for that one */
    internal val functor: Int = 0,
) {
    /** The identifier of the record, as the identifiers of a context know it (see `context_id/2`); -1 for none */
    @InternalArg2pApi
    var identifier: Int = -1

    /** The term, decoded once */
    val term: Term get() = decoded ?: (source ?: decode(root, HashMap())).also { decoded = it }
    private var decoded: Term? = null

    /** The term of the word [w] of the record, decoded now and not kept */
    @InternalArg2pApi
    fun termAt(w: Int): Term = decode(w, HashMap())

    /** The key of the term of the word [w] of the record: the same for two terms up to the names of their variables */
    @InternalArg2pApi
    fun keyAt(w: Int): RecordKey = TermFlattener(program!!, words.size + 1).keyOf(words, w)

    /** The word of the argument [i] of the head (only with a program) */
    @InternalArg2pApi
    fun argumentWord(i: Int): Int = words[payload(head) + (if (tag(head) == LIST) i else i + 1)]

    /** The argument [i] of the head as an identifier, a small integer not negative; -1 if it is none (only with a program) */
    @InternalArg2pApi
    fun idAt(i: Int): Int {
        val w = argumentWord(i)
        return if (tag(w) == INT && payload(w) >= 0) payload(w) else -1
    }

    /** The argument [i] of the head as the term the program keeps for an atom; null if it is none (only with a program) */
    @InternalArg2pApi
    fun atomAt(i: Int): Term? {
        val w = argumentWord(i)
        return if (tag(w) == ATOM) program!!.atomTerm(payload(w)) else null
    }

    /** Whether the word [w] of the record is a list cell */
    @InternalArg2pApi
    fun isCell(w: Int): Boolean = tag(w) == LIST

    /** The head and the tail of the list cell [w] of the record */
    @InternalArg2pApi
    fun cellHead(w: Int): Int = words[payload(w)]

    @InternalArg2pApi
    fun cellTail(w: Int): Int = words[payload(w) + 1]

    /** Whether the word [w] of the record is an atom or a small integer: equal words are equal terms */
    @InternalArg2pApi
    fun isSymbol(w: Int): Boolean = tag(w) == ATOM || tag(w) == INT

    /** The item [i] of the list [list] of the record, which has it */
    @InternalArg2pApi
    fun itemAt(
        list: Int,
        i: Int,
    ): Int {
        var cell = list
        repeat(i) { cell = cellTail(cell) }
        return cellHead(cell)
    }

    /** The clause: `Head :- Body` read as a rule, anything else as a fact */
    val clause: Clause get() = clauseRead ?: readClause().also { clauseRead = it }
    private var clauseRead: Clause? = null

    private fun readClause(): Clause {
        val t = term
        return if (t is Struct && t.functor == ":-" && t.arity == 2) Rule.of(t[0].castToStruct(), t[1]) else Fact.of(t.castToStruct())
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
    @InternalArg2pApi
    val key: RecordKey get() = keyRead ?: keyAt(root).also { keyRead = it }
    private var keyRead: RecordKey? = null

    private fun decode(
        w: Int,
        variables: HashMap<Int, Var>,
    ): Term =
        object : TermReader(program!!) {
            override fun cell(address: Int): Int = words[address]

            override fun variable(address: Int): Var = Var.of("V")

            override fun functorOf(
                w: Int,
                address: Int,
            ): Int = if (functor != 0 && w == root) functor else words[address]
        }.read(w, variables)

    /** The term of the word [w] of the record, as a record of its own: its cells copied, nothing decoded */
    @InternalArg2pApi
    fun sub(w: Int): Record {
        var out = IntArray(16)
        var top = 0
        val moved = HashMap<Int, Int>()

        fun alloc(cells: Int): Int {
            if (top + cells > out.size) out = out.copyOf(maxOf(out.size * 2, top + cells))
            return top.also { top += cells }
        }

        fun copy(word: Int): Int =
            when (tag(word)) {
                REF -> {
                    val a = payload(word)
                    if (words[a] != word) {
                        copy(words[a])
                    } else {
                        ref(moved.getOrPut(a) { alloc(1).also { out[it] = ref(it) } })
                    }
                }
                STR -> {
                    val a = payload(word)
                    moved[a]?.let { return word(STR, it) }
                    val f = if (functor != 0 && word == root) functor else words[a]
                    val n = program!!.arity(f)
                    val at = alloc(n + 1)
                    moved[a] = at
                    out[at] = f
                    for (i in 1..n) {
                        val argument = copy(words[a + i])
                        out[at + i] = argument
                    }
                    word(STR, at)
                }
                LIST -> {
                    val a = payload(word)
                    moved[a]?.let { return word(LIST, it) }
                    val at = alloc(2)
                    moved[a] = at
                    val head = copy(words[a])
                    out[at] = head
                    val tail = copy(words[a + 1])
                    out[at + 1] = tail
                    word(LIST, at)
                }
                else -> word
            }
        val subRoot = copy(w)
        return Record(program, out.copyOf(top), subRoot, null)
    }

    companion object {
        /** `f(Parts...)` in the words of [program], [functor] the word of f: the words of each part copied after its cell */
        @InternalArg2pApi
        fun compound(
            program: Program,
            functor: Int,
            parts: List<Record>,
        ): Record {
            val n = parts.size
            val out = IntArray(1 + n + parts.sumOf { it.words.size })
            out[0] = functor
            var offset = 1 + n
            parts.forEachIndexed { i, part ->
                require(
                    program.readsWordsOf(part.program) && part.functor == 0,
                ) { "A part in the words of another program, or renamed: ${part.term}" }
                out[1 + i] = shifted(part.root, offset)
                for (k in part.words.indices) out[offset + k] = shifted(part.words[k], offset)
                offset += part.words.size
            }
            return Record(program, out, word(STR, 0), null)
        }

        /** [term] as a record holding the term, encoded by the store that keeps it */
        fun of(term: Term): Record = Record(null, IntArray(0), 0, term)

        /** [term] as the words of [program] */
        @InternalArg2pApi
        fun of(
            term: Term,
            program: Program,
        ): Record {
            val encoder = Encoder(program)
            val root = encoder.write(term, HashMap())
            return Record(program, encoder.words.copyOf(encoder.top), root, null)
        }

        /** [record] in the words of [program], itself if it is there already */
        @InternalArg2pApi
        fun encoded(
            record: Record,
            program: Program,
        ): Record =
            when {
                record.program === program -> record
                // (the words of another program of the session: the same record, in this program, its identifier kept)
                program.readsWordsOf(record.program) ->
                    Record(program, record.words, record.root, null, record.functor).also { it.identifier = record.identifier }
                else -> of(record.term, program)
            }
    }

    private class Encoder(
        program: Program,
    ) : TermWriter(program) {
        var words = IntArray(16)
        var top = 0

        override fun alloc(cells: Int): Int {
            if (top + cells > words.size) words = words.copyOf(maxOf(words.size * 2, top + cells))
            val start = top
            top += cells
            return start
        }

        override fun put(
            address: Int,
            w: Int,
        ) {
            words[address] = w
        }

        override fun variable(term: Var): Int {
            val a = alloc(1)
            words[a] = ref(a)
            return a
        }
    }
}

/** A term up to the names of its variables: see [Record.key] */
@InternalArg2pApi
class RecordKey(
    private val flat: IntArray,
    private val size: Int,
) {
    private val hash: Int

    init {
        var h = 1
        for (i in 0 until size) h = 31 * h + flat[i]
        // (mixed: terms differing by one symbol in several places differ by a multiple of a power of two, the same low
        // bits a hash table picks its bucket by, and the table of a session holds the records of every node)
        h = (h xor (h ushr 16)) * -2048144789
        h = (h xor (h ushr 13)) * -1028477387
        hash = h xor (h ushr 16)
    }

    override fun hashCode(): Int = hash

    override fun equals(other: Any?): Boolean {
        if (other !is RecordKey || other.size != size || other.hash != hash) return false
        for (i in 0 until size) if (other.flat[i] != flat[i]) return false
        return true
    }
}
