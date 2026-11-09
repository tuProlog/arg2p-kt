package it.unibo.tuprolog.argumentation.engine.words

import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi
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
@InternalArg2pApi
class Symbols private constructor(
    private val concurrent: Boolean,
) {
    /** The names of the atoms, by id */
    val atoms = GrowOnlyTable<String>()
    private val atomIds = HashMap<String, Int>()

    /** The functors: the atom naming each, and its arity, by id */
    val functorNames = GrowOnlyTable<Int>()
    var functorArities = IntArray(1 shl 10)
        private set
    private val functorIds = HashMap<Int, Int>()
    private val wideFunctorIds = HashMap<Long, Int>()

    // (the floats as tuProlog reads them: a float read from "61.167" and the double 61.167 are two values, and the
    // first must come out as it went in)
    val floats = GrowOnlyTable<Real>()
    private val floatIds = HashMap<Real, Int>()
    val bigs = GrowOnlyTable<Long>()
    private val bigIds = HashMap<Long, Int>()

    /** The integers beyond 64 bits: a large integer word with a negative payload, -1 - index, stands for one */
    val huges = GrowOnlyTable<BigInteger>()
    private val hugeIds = HashMap<BigInteger, Int>()

    // The symbols of the base program when this table was made, shared by the tables of the programs as they are never
    // changed: putting them in a map for each solver hashed all the names of all the modules
    private var baseAtomIds: Map<String, Int>? = null
    private var baseFunctorIds: Map<Int, Int>? = null
    private var sharedAtoms: Map<String, Int>? = null
    private var sharedFunctors: Map<Int, Int>? = null

    // What a table made from this one takes as its base: those of its own base and its own (the base of a session is a table
    // made from another), kept until they change
    private fun <K> toShare(
        base: Map<K, Int>?,
        own: Map<K, Int>,
        kept: Map<K, Int>?,
    ): Map<K, Int> =
        kept?.takeIf { it.size == (base?.size ?: 0) + own.size }
            ?: HashMap(base ?: emptyMap()).also { it.putAll(own) }

    /** The id of the atom [name], added if new */
    fun atom(name: String): Int =
        baseAtomIds?.get(name) ?: lockedIf(concurrent, this) {
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
            return lockedIf(concurrent, this) { functorIds.getOrPut(key) { newFunctor(nameAtom, arity) } }
        }
        return lockedIf(
            concurrent,
            this,
        ) { wideFunctorIds.getOrPut((nameAtom.toLong() shl 32) or arity.toLong()) { newFunctor(nameAtom, arity) } }
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

    fun float(value: Real): Int = lockedIf(concurrent, this) { floatIds.getOrPut(value) { floats.add(value) } }

    fun big(value: Long): Int = lockedIf(concurrent, this) { bigIds.getOrPut(value) { bigs.add(value) } }

    fun huge(value: BigInteger): Int = lockedIf(concurrent, this) { hugeIds.getOrPut(value) { huges.add(value) } }

    /** Whether the programs of a session share the table */
    val isShared: Boolean get() = concurrent

    /** The table of a program derived from one with this table: this one if shared, a copy otherwise */
    fun forDerived(): Symbols = if (concurrent) this else of(this)

    companion object {
        /** A table of its own for a program made from [base] (none for the base itself): the symbols of the base as they are now */
        fun of(base: Symbols?): Symbols = Symbols(concurrent = false).also { if (base != null) it.copyOf(base) }

        /** A table the programs of a session share, from the symbols of [base] as they are now */
        fun shared(base: Symbols): Symbols = Symbols(concurrent = true).also { it.copyOf(base) }
    }

    // (under the lock of [base] if shared: the other programs of its session may be adding to it)
    private fun copyOf(base: Symbols) = lockedIf(base.concurrent, base) { copyOfNow(base) }

    private fun copyOfNow(base: Symbols) {
        atoms.addAll(base.atoms)
        baseAtomIds = base.toShare(base.baseAtomIds, base.atomIds, base.sharedAtoms).also { base.sharedAtoms = it }
        functorNames.addAll(base.functorNames)
        functorArities = base.functorArities.copyOf()
        baseFunctorIds = base.toShare(base.baseFunctorIds, base.functorIds, base.sharedFunctors).also { base.sharedFunctors = it }
        wideFunctorIds.putAll(base.wideFunctorIds)
        floats.addAll(base.floats)
        floatIds.putAll(base.floatIds)
        bigs.addAll(base.bigs)
        bigIds.putAll(base.bigIds)
        huges.addAll(base.huges)
        hugeIds.putAll(base.hugeIds)
    }
}
