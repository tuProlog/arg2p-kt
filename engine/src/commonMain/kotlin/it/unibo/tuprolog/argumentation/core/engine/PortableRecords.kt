package it.unibo.tuprolog.argumentation.core.engine

import it.unibo.tuprolog.argumentation.core.engine.vm.Program
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.ATOM
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.BIG
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.FLT
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.FUN
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.INT
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.LIST
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.REF
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.STR
import it.unibo.tuprolog.argumentation.core.engine.vm.payload
import it.unibo.tuprolog.argumentation.core.engine.vm.tag
import it.unibo.tuprolog.argumentation.core.engine.vm.word
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Real
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import org.gciatto.kt.math.BigInteger
import it.unibo.tuprolog.core.List as LogicList

/**
 * Records of one program written for any other: their words as they are, but for the symbols (atoms, functors, floats,
 * large integers), numbered within the batch and named in its tables. The program that writes them reads its own tables
 * alone, and the one that reads them ([records]) looks each symbol of the batch up once: no term is made on either
 * side, and two nodes on two threads never read each other's tables. The words of a record are relative to its own
 * start, so they are copied as they are.
 */
class PortableRecords internal constructor(
    // the words of all the records, one after the other, and where each starts (size + 1 entries)
    internal val words: IntArray,
    internal val starts: IntArray,
    // the root word of each record, relative to its start, and the functor it is named under (see Record.named): a
    // functor of the batch plus one, 0 for its own
    internal val roots: IntArray,
    internal val renamed: IntArray,
    internal val atoms: Array<String>,
    // the functors of the batch: the atom of the batch naming each, and its arity
    internal val functorNames: IntArray,
    internal val functorArities: IntArray,
    internal val floats: Array<Real>,
    internal val integers: Array<BigInteger>,
) {
    /** How many records the batch holds */
    val size: Int get() = roots.size

    /** The term of the record [i], read off the tables of the batch alone (no program: from any thread) */
    fun term(i: Int): Term {
        val start = starts[i]
        val variables = HashMap<Int, Var>()

        fun decode(w: Int): Term =
            when (tag(w)) {
                REF -> {
                    val a = payload(w)
                    val bound = words[start + a]
                    if (bound != w) decode(bound) else variables.getOrPut(a) { Var.of("V") }
                }
                ATOM -> Atom.of(atoms[payload(w)])
                INT -> Integer.of(payload(w))
                BIG -> Integer.of(integers[payload(w)])
                FLT -> floats[payload(w)]
                STR -> {
                    val a = payload(w)
                    val f = if (renamed[i] != 0 && w == roots[i]) renamed[i] - 1 else payload(words[start + a])
                    Struct.of(atoms[functorNames[f]], List(functorArities[f]) { decode(words[start + a + 1 + it]) })
                }
                LIST -> {
                    val heads = ArrayList<Term>()
                    var a = payload(w)
                    while (true) {
                        heads.add(decode(words[start + a]))
                        val t = words[start + a + 1]
                        if (tag(t) == LIST) a = payload(t) else return LogicList.from(heads, decode(t))
                    }
                    @Suppress("UNREACHABLE_CODE")
                    throw IllegalStateException()
                }
                else -> throw IllegalStateException("A functor as a term")
            }
        return decode(roots[i])
    }

    /** The records in the words of [program], each symbol of the batch looked up there once */
    internal fun records(program: Program): List<Record> = records(program, IntArray(size) { it })

    /** The records of [indices] in the words of [program], in that order */
    internal fun records(
        program: Program,
        indices: IntArray,
    ): List<Record> {
        val atomWords = IntArray(atoms.size) { program.atomWord(atoms[it]) }
        val functorWords =
            IntArray(functorNames.size) { word(FUN, program.functor(payload(atomWords[functorNames[it]]), functorArities[it])) }
        val floatWords = IntArray(floats.size) { program.floatWord(floats[it]) }
        val integerWords = IntArray(integers.size) { program.integerWord(integers[it]) }

        fun read(w: Int): Int =
            when (tag(w)) {
                ATOM -> atomWords[payload(w)]
                FUN -> functorWords[payload(w)]
                FLT -> floatWords[payload(w)]
                BIG -> integerWords[payload(w)]
                else -> w
            }
        return List(indices.size) { at ->
            val i = indices[at]
            val start = starts[i]
            val own = IntArray(starts[i + 1] - start) { read(words[start + it]) }
            // (a term with no cells, an atom or a number, is its root alone)
            Record(program, own, read(roots[i]), null, if (renamed[i] == 0) 0 else functorWords[renamed[i] - 1])
        }
    }

    companion object {
        /** [records], each in the words of a program (the same or not), written for any other */
        internal fun of(records: List<Record>): PortableRecords {
            val writer = Writer()
            records.forEach { writer.add(it) }
            return writer.batch()
        }
    }

    // The tables of the batch, filled as the records are written: a symbol of a program numbered once
    private class Writer {
        private val words = ArrayList<Int>()
        private val starts = ArrayList<Int>()
        private val roots = ArrayList<Int>()
        private val renamed = ArrayList<Int>()
        private val atoms = ArrayList<String>()
        private val atomIndex = HashMap<String, Int>()
        private val functorNames = ArrayList<Int>()
        private val functorArities = ArrayList<Int>()
        private val functorIndex = HashMap<Long, Int>()
        private val floats = ArrayList<Real>()
        private val floatIndex = HashMap<Real, Int>()
        private val integers = ArrayList<BigInteger>()
        private val integerIndex = HashMap<BigInteger, Int>()

        private fun atom(name: String): Int =
            atomIndex.getOrPut(name) {
                atoms += name
                atoms.size - 1
            }

        private fun functor(
            program: Program,
            functorWord: Int,
        ): Int {
            val name = atom(program.functorName(functorWord))
            val arity = program.arity(functorWord)
            return functorIndex.getOrPut((name.toLong() shl 32) or arity.toLong()) {
                functorNames += name
                functorArities += arity
                functorNames.size - 1
            }
        }

        fun add(record: Record) {
            val program = record.program ?: throw IllegalArgumentException("A record not encoded in a program: ${record.term}")
            starts += words.size
            roots += written(program, record.root)
            renamed += if (record.functor == 0) 0 else functor(program, record.functor) + 1
            for (w in record.words) words += written(program, w)
        }

        // A word of [program] as the batch writes it: its symbol numbered within the batch
        private fun written(
            program: Program,
            w: Int,
        ): Int =
            when (tag(w)) {
                ATOM -> word(ATOM, atom(program.atoms[payload(w)]))
                FUN -> word(FUN, functor(program, w))
                FLT -> {
                    val value = program.floats[payload(w)]
                    word(FLT, floatIndex.getOrPut(value) { floats.size.also { floats += value } })
                }
                BIG -> {
                    val value = program.bigInteger(payload(w))
                    word(BIG, integerIndex.getOrPut(value) { integers.size.also { integers += value } })
                }
                else -> w
            }

        fun batch(): PortableRecords {
            starts += words.size
            return PortableRecords(
                words.toIntArray(),
                starts.toIntArray(),
                roots.toIntArray(),
                renamed.toIntArray(),
                atoms.toTypedArray(),
                functorNames.toIntArray(),
                functorArities.toIntArray(),
                floats.toTypedArray(),
                integers.toTypedArray(),
            )
        }
    }
}
