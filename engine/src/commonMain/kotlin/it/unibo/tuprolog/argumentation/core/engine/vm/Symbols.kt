package it.unibo.tuprolog.argumentation.core.engine.vm

import it.unibo.tuprolog.core.Real
import org.gciatto.kt.math.BigInteger

/**
 * The symbol tables of programs: atoms, functors (an atom and an arity), floats and large integers, each once, so that
 * equal constants are equal words. A program has a table of its own, made from the base program's ([of]), or shares a
 * table with the other programs of a session ([shared]): the words of any of them are then words of every other, and
 * the records of one are read by another as they are.
 *
 * The tables only grow. A shared table interns under a lock (the symbols of the base, the most of them, are found
 * before it, in maps no one changes), and is read without one: a word of a symbol reaches another thread through a
 * message, after the symbol was added.
 */
internal class Symbols private constructor(
    private val concurrent: Boolean,
) {
    /** The names of the atoms, by id */
    val atoms = Table<String>()
    private val atomIds = HashMap<String, Int>()

    /** The functors: the atom naming each, and its arity, by id */
    val functorNames = Table<Int>()
    var functorArities = IntArray(1 shl 10)
        private set
    private val functorIds = HashMap<Int, Int>()
    private val wideFunctorIds = HashMap<Long, Int>()

    // (the floats as tuProlog reads them: a float read from "61.167" and the double 61.167 are two values, and the
    // first must come out as it went in)
    val floats = Table<Real>()
    private val floatIds = HashMap<Real, Int>()
    val bigs = Table<Long>()
    private val bigIds = HashMap<Long, Int>()

    /** The integers beyond 64 bits: a large integer word with a negative payload, -1 - index, stands for one */
    val huges = Table<BigInteger>()
    private val hugeIds = HashMap<BigInteger, Int>()

    // The symbols of the base program when this table was made, shared by the tables of the programs as they are never
    // changed: putting them in a map for each solver hashed all the names of all the modules
    private var baseAtomIds: Map<String, Int>? = null
    private var baseFunctorIds: Map<Int, Int>? = null
    private var sharedAtoms: Map<String, Int>? = null
    private var sharedFunctors: Map<Int, Int>? = null

    private fun atomsToShare(): Map<String, Int> =
        sharedAtoms?.takeIf { it.size == atomIds.size } ?: HashMap(atomIds).also { sharedAtoms = it }

    private fun functorsToShare(): Map<Int, Int> =
        sharedFunctors?.takeIf { it.size == functorIds.size } ?: HashMap(functorIds).also { sharedFunctors = it }

    /** The id of the atom [name], added if new */
    fun atom(name: String): Int =
        baseAtomIds?.get(name) ?: changing {
            atomIds.getOrPut(name) { atoms.add(name) }
        }

    /** The id of the functor of the atom [nameAtom] and [arity], added if new */
    fun functor(
        nameAtom: Int,
        arity: Int,
    ): Int {
        if (nameAtom < 1 shl 21 && arity < 1 shl 10) {
            val key = (nameAtom shl 10) or arity
            baseFunctorIds?.get(key)?.let { return it }
            return changing { functorIds.getOrPut(key) { newFunctor(nameAtom, arity) } }
        }
        return changing { wideFunctorIds.getOrPut((nameAtom.toLong() shl 32) or arity.toLong()) { newFunctor(nameAtom, arity) } }
    }

    private fun newFunctor(
        nameAtom: Int,
        arity: Int,
    ): Int {
        val id = functorNames.add(nameAtom)
        if (id == functorArities.size) functorArities = functorArities.copyOf(id * 2)
        functorArities[id] = arity
        return id
    }

    fun float(value: Real): Int = changing { floatIds.getOrPut(value) { floats.add(value) } }

    fun big(value: Long): Int = changing { bigIds.getOrPut(value) { bigs.add(value) } }

    fun huge(value: BigInteger): Int = changing { hugeIds.getOrPut(value) { huges.add(value) } }

    private inline fun <T> changing(crossinline change: () -> T): T = if (concurrent) locked(this) { change() } else change()

    companion object {
        /** A table of its own for a program made from [base] (none for the base itself): the symbols of the base as they are now */
        fun of(base: Symbols?): Symbols = Symbols(concurrent = false).also { if (base != null) it.copyOf(base) }

        /** A table the programs of a session share, from the symbols of [base] as they are now */
        fun shared(base: Symbols): Symbols = Symbols(concurrent = true).also { it.copyOf(base) }
    }

    private fun copyOf(base: Symbols) {
        atoms.addAll(base.atoms)
        baseAtomIds = base.atomsToShare()
        functorNames.addAll(base.functorNames)
        functorArities = base.functorArities.copyOf()
        baseFunctorIds = base.functorsToShare()
        wideFunctorIds.putAll(base.wideFunctorIds)
        floats.addAll(base.floats)
        floatIds.putAll(base.floatIds)
        bigs.addAll(base.bigs)
        bigIds.putAll(base.bigIds)
        huges.addAll(base.huges)
        hugeIds.putAll(base.hugeIds)
    }

    /** A table that only grows: read by index, without a lock */
    internal class Table<T> {
        private var items = arrayOfNulls<Any>(64)

        /** How many items it holds */
        var size = 0
            private set

        @Suppress("UNCHECKED_CAST")
        operator fun get(index: Int): T = items[index] as T

        /** [item] added, its index */
        fun add(item: T): Int {
            if (size == items.size) items = items.copyOf(size * 2)
            items[size] = item
            return size++
        }

        fun addAll(other: Table<T>) {
            for (i in 0 until other.size) add(other[i])
        }
    }
}
