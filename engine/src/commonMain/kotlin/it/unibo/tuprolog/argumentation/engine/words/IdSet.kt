package it.unibo.tuprolog.argumentation.engine.words

import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi

/** A set of identifiers (small integers, see contextId), kept in the order they were added */
@InternalArg2pApi
class IdSet private constructor(
    private var items: IntArray,
    size: Int,
    private var bits: IntArray,
) {
    constructor(capacity: Int = 8) : this(IntArray(maxOf(capacity, 1)), 0, IntArray(1))

    var size: Int = size
        private set

    operator fun get(index: Int): Int = items[index]

    operator fun contains(id: Int): Boolean {
        val block = id ushr 5
        return block < bits.size && (bits[block] ushr id) and 1 != 0
    }

    /** Whether [id] was added now, rather than being here already */
    fun add(id: Int): Boolean {
        if (id in this) return false
        val block = id ushr 5
        if (block >= bits.size) bits = bits.copyOf(maxOf(block + 1, bits.size * 2))
        bits[block] = bits[block] or (1 shl id)
        if (size == items.size) items = items.copyOf(size * 2)
        items[size++] = id
        return true
    }

    fun addAll(ids: IntArray): IdSet {
        for (id in ids) add(id)
        return this
    }

    fun copy(): IdSet = IdSet(items.copyOf(), size, bits.copyOf())

    fun toIntArray(): IntArray = items.copyOf(size)

    /** The identifiers also in [other], in the order of this set */
    fun intersect(other: IdSet): IdSet {
        val result = IdSet(minOf(size, other.size))
        for (i in 0 until size) if (items[i] in other) result.add(items[i])
        return result
    }

    fun containsAll(other: IdSet): Boolean {
        if (other.size > size) return false
        for (i in 0 until other.size) if (other.items[i] !in this) return false
        return true
    }

    /** The same identifiers, whatever the order */
    fun sameAs(other: IdSet): Boolean = size == other.size && containsAll(other)

    companion object {
        /** Empty, not to be added to */
        val NONE = IdSet()

        fun of(ids: IntArray): IdSet = IdSet(ids.size).addAll(ids)
    }
}
