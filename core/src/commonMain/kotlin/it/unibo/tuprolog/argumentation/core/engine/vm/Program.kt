package it.unibo.tuprolog.argumentation.core.engine.vm

import it.unibo.tuprolog.argumentation.core.engine.vm.Words.ATOM
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.BIG
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.FLT
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.FUN
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.INT
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.LIST
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.REF
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.STR
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.payload
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.tag
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.word
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Cons
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Real
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.theory.Theory

/**
 * A program of the machine (spike): the symbol tables (atoms, functors, floats and large integers, each once, so that
 * equal constants are equal words), the code of the clauses (their terms as words in a code heap, their variables as
 * slots) and the predicates, each with its clauses and their first-argument keys.
 */
internal class Program {
    val atoms = ArrayList<String>()
    private val atomIds = HashMap<String, Int>()

    /** The functors: name atom and arity, by id */
    val functorNames = ArrayList<Int>()
    val functorArities = ArrayList<Int>()
    private val functorIds = HashMap<Long, Int>()

    val floats = ArrayList<Double>()
    private val floatIds = HashMap<Double, Int>()
    val bigs = ArrayList<Long>()
    private val bigIds = HashMap<Long, Int>()

    /** The code heap: the terms of the clauses */
    var code = IntArray(1 shl 12)
    var codeTop = 0

    val predicates = HashMap<Int, Predicate>()

    val emptyList: Int = atomWord("[]")
    val functorConj: Int = functor(",", 2)
    val functorCall: Int = functor("call", 1)

    class ClauseCode(
        val key: Int,
        val variables: Int,
        val headArgs: IntArray,
        val body: IntArray,
    )

    class Predicate(
        val functor: Int,
    ) {
        val clauses = ArrayList<ClauseCode>()
    }

    fun atom(name: String): Int =
        atomIds.getOrPut(name) {
            atoms.add(name)
            atoms.size - 1
        }

    fun atomWord(name: String): Int = word(ATOM, atom(name))

    fun functor(
        name: String,
        arity: Int,
    ): Int = functor(atom(name), arity)

    fun functor(
        nameAtom: Int,
        arity: Int,
    ): Int {
        val key = (nameAtom.toLong() shl 32) or arity.toLong()
        return functorIds.getOrPut(key) {
            functorNames.add(nameAtom)
            functorArities.add(arity)
            functorNames.size - 1
        }
    }

    fun functorWord(
        name: String,
        arity: Int,
    ): Int = word(FUN, functor(name, arity))

    fun arity(functorWord: Int): Int = functorArities[payload(functorWord)]

    fun functorName(functorWord: Int): String = atoms[functorNames[payload(functorWord)]]

    fun floatWord(value: Double): Int =
        word(
            FLT,
            floatIds.getOrPut(value) {
                floats.add(value)
                floats.size - 1
            },
        )

    fun bigWord(value: Long): Int =
        word(
            BIG,
            bigIds.getOrPut(value) {
                bigs.add(value)
                bigs.size - 1
            },
        )

    /** The word of an integer: a small one as it is, a large one by the table */
    fun intWord(value: Long): Int = if (Words.fitsInt(value)) word(INT, value.toInt()) else bigWord(value)

    private fun codeAlloc(cells: Int): Int {
        if (codeTop + cells > code.size) code = code.copyOf(maxOf(code.size * 2, codeTop + cells))
        val start = codeTop
        codeTop += cells
        return start
    }

    /** Adds the clauses of [theory] */
    fun add(theory: Theory) {
        for (clause in theory.clauses) add(clause)
    }

    fun add(clause: Clause) {
        val head = clause.head ?: return
        val slots = HashMap<Var, Int>()
        val headArgs = IntArray(head.arity) { toCode(head[it], slots) }
        val goals = ArrayList<Term>()
        flatten(clause.body, goals)
        val body = IntArray(goals.size) { toCode(goals[it], slots) }
        val key = if (headArgs.isEmpty()) 0 else keyOfCode(headArgs[0])
        val functor = functor(head.functor, head.arity)
        predicates.getOrPut(functor) { Predicate(functor) }.clauses.add(ClauseCode(key, slots.size, headArgs, body))
    }

    private fun flatten(
        body: Term,
        into: ArrayList<Term>,
    ) {
        var current = body
        while (current is Struct && current.functor == "," && current.arity == 2) {
            flatten(current[0], into)
            current = current[1]
        }
        if (current.isTrue) return
        into.add(if (current is Var) Struct.of("call", current) else current)
    }

    /** The first-argument key of a code word: 0 for a variable (matches anything), the constant or functor word otherwise */
    fun keyOfCode(w: Int): Int =
        when (tag(w)) {
            REF -> 0
            STR -> code[payload(w)]
            LIST -> LIST_KEY
            else -> w
        }

    /** [term] as code: its variables as the slots of [slots], given in order of appearance */
    fun toCode(
        term: Term,
        slots: HashMap<Var, Int>,
    ): Int =
        when (term) {
            is Var -> word(REF, slots.getOrPut(term) { slots.size })
            is Integer -> intWord(term.value.toLongExact())
            is Real -> floatWord(term.value.toDouble())
            is Atom -> atomWord(term.value)
            is Cons -> {
                val start = codeAlloc(2)
                val head = toCode(term.head, slots)
                val tail = toCode(term.tail, slots)
                code[start] = head
                code[start + 1] = tail
                word(LIST, start)
            }
            is Struct -> {
                val n = term.arity
                val start = codeAlloc(n + 1)
                code[start] = word(FUN, functor(term.functor, n))
                for (i in 0 until n) {
                    val arg = toCode(term[i], slots)
                    code[start + 1 + i] = arg
                }
                word(STR, start)
            }
            else -> throw IllegalArgumentException("not a term of the machine: $term")
        }

    companion object {
        /** The first-argument key of a list cell */
        const val LIST_KEY = (0 shl Words.TAG_BITS) or LIST
    }
}

// (The spike keeps integers within a Long)
private fun Integer.toLongExact(): Long = value.toLong()
