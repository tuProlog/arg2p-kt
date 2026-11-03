package it.unibo.tuprolog.argumentation.core.runtime

import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var

/**
 * The bindings of a few variables (arg2p): a resolution binds the variables of a clause and of its goal, a handful,
 * which it kept in hash maps allocated, filled with nodes and resized for each resolution (a fifth of the
 * allocations of a query). The bindings are kept in two arrays and found by a scan, by an index beyond
 * [LINEAR] of them. A variable is the same as a key when it is the key, or has its name and is equal to it (two
 * variables are equal by name and identifier: the scan compares the names at hand, and the identifiers, through
 * the complete names the variables build, only for a name met twice).
 */
internal class VarMap : AbstractMap<Var, Term>() {
    private var variables = arrayOfNulls<Var>(INITIAL)
    private var terms = arrayOfNulls<Term>(INITIAL)
    private var index: HashMap<Var, Int>? = null

    override var size: Int = 0
        private set

    override fun isEmpty(): Boolean = size == 0

    override fun containsKey(key: Var): Boolean = indexOf(key) >= 0

    override fun get(key: Var): Term? {
        val i = indexOf(key)
        return if (i < 0) null else terms[i]
    }

    /** Binds [key] to [value], in the place of its binding if it has one */
    fun put(
        key: Var,
        value: Term,
    ) {
        val i = indexOf(key)
        if (i >= 0) {
            terms[i] = value
            return
        }
        if (size == variables.size) {
            variables = variables.copyOf(size * 2)
            terms = terms.copyOf(size * 2)
        }
        variables[size] = key
        terms[size] = value
        index?.put(key, size)
        size++
        if (index == null && size > LINEAR) {
            index = HashMap<Var, Int>(size * 4).also { for (j in 0 until size) it[variables[j]!!] = j }
        }
    }

    /** The variable bound at [position], in the order the bindings were added */
    fun keyAt(position: Int): Var = variables[position]!!

    /** The term bound at [position], in the order the bindings were added */
    fun valueAt(position: Int): Term = terms[position]!!

    private fun indexOf(key: Var): Int {
        index?.let { return it[key] ?: -1 }
        for (i in 0 until size) {
            val k = variables[i]!!
            if (k === key || (k.name == key.name && k == key)) return i
        }
        return -1
    }

    // The entries, as read by the unifiers built from the map (tuProlog copies any map it is given)
    override val entries: Set<Map.Entry<Var, Term>>
        get() = Entries()

    private inner class Entries : AbstractSet<Map.Entry<Var, Term>>() {
        override val size: Int
            get() = this@VarMap.size

        override fun iterator(): Iterator<Map.Entry<Var, Term>> =
            object : Iterator<Map.Entry<Var, Term>> {
                private var position = 0

                override fun hasNext(): Boolean = position < this@VarMap.size

                override fun next(): Map.Entry<Var, Term> {
                    val i = position++
                    return Binding(variables[i]!!, terms[i]!!)
                }
            }
    }

    private class Binding(
        override val key: Var,
        override val value: Term,
    ) : Map.Entry<Var, Term>

    private companion object {
        const val INITIAL = 4
        const val LINEAR = 12
    }
}
