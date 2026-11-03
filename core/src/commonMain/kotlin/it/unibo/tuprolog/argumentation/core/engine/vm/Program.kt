package it.unibo.tuprolog.argumentation.core.engine.vm

import it.unibo.tuprolog.argumentation.core.engine.vm.Words.ATOM
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.BIG
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.FLT
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.FUN
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.INT
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.LIST
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.payload
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.word
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.theory.Theory

/**
 * A program of the machine (spike): the symbol tables (atoms, functors, floats and large integers, each once, so that
 * equal constants are equal words), the code array the clauses are compiled into (see [Compiler]) and the
 * predicates, each with its clauses and their first-argument keys.
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

    /** The code of the clauses: instructions and their operands */
    var code = IntArray(1 shl 14)
    var codeTop = 0

    /** The predicates by functor id (null where the program has none) */
    var predicateTable = arrayOfNulls<Predicate>(1 shl 8)
        private set

    /** The highest register any clause uses */
    var maxRegister = 0

    private val compiler = Compiler(this)

    class ClauseCode(
        val key: Int,
        /** The address of the first instruction */
        val start: Int,
        // The clause as given, for the dynamic ones (retracted by equality)
        val source: Clause?,
    )

    class Predicate(
        val functor: Int,
    ) {
        val clauses = ArrayList<ClauseCode>()
    }

    val emptyList: Int = atomWord("[]")

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
            val id = functorNames.size - 1
            if (id >= predicateTable.size) predicateTable = predicateTable.copyOf(predicateTable.size * 2)
            id
        }
    }

    fun functorWord(
        name: String,
        arity: Int,
    ): Int = word(FUN, functor(name, arity))

    fun arity(functorWord: Int): Int = functorArities[payload(functorWord)]

    fun functorName(functorWord: Int): String = atoms[functorNames[payload(functorWord)]]

    fun nameOfFunctor(functor: Int): String = atoms[functorNames[functor]]

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

    // ---- code

    /** Appends an instruction word (an opcode or an operand); the address it got */
    fun emit(w: Int): Int {
        if (codeTop == code.size) code = code.copyOf(code.size * 2)
        code[codeTop] = w
        return codeTop++
    }

    fun emit(
        opcode: Int,
        a: Int,
    ) {
        emit(opcode)
        emit(a)
    }

    fun emit(
        opcode: Int,
        a: Int,
        b: Int,
    ) {
        emit(opcode)
        emit(a)
        emit(b)
    }

    /** Adds the clauses of [theory] */
    fun add(theory: Theory) {
        for (clause in theory.clauses) add(clause)
    }

    /** Adds [clauses] */
    fun add(clauses: Iterable<Clause>) {
        for (clause in clauses) add(clause)
    }

    /** Adds [clause], last among those of its predicate (first if [onTop]); [dynamic] ones can be removed */
    fun add(
        clause: Clause,
        dynamic: Boolean = false,
        onTop: Boolean = false,
    ) {
        val head = clause.head ?: return
        val functor = functor(head.functor, head.arity)
        val compiled = compiler.compile(clause, if (dynamic) clause else null)
        val predicate = predicateTable[functor] ?: Predicate(functor).also { predicateTable[functor] = it }
        if (onTop) predicate.clauses.add(0, compiled) else predicate.clauses.add(compiled)
    }

    /** The code of [query] as a clause whose head holds its variables: the address of its first instruction */
    fun compileQuery(query: it.unibo.tuprolog.core.Struct): Compiler.Query = compiler.compileQuery(query)

    /** Removes the first dynamic clause equal to [clause] (variables apart); false if there is none */
    fun remove(clause: Clause): Boolean {
        val head = clause.head ?: return false
        val predicate = predicateTable[functor(head.functor, head.arity)] ?: return false
        val index = predicate.clauses.indexOfFirst { it.source != null && it.source.equals(clause, false) }
        if (index < 0) return false
        predicate.clauses.removeAt(index)
        return true
    }

    companion object {
        /** The first-argument key of a list cell */
        const val LIST_KEY = (0 shl Words.TAG_BITS) or LIST
    }
}
