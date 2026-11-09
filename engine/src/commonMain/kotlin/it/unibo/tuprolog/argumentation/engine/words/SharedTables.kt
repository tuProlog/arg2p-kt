package it.unibo.tuprolog.argumentation.engine.words

import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi

/** A table that only grows: read by index, without a lock */
@InternalArg2pApi
class GrowOnlyTable<T> {
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

    fun addAll(other: GrowOnlyTable<T>) {
        for (i in 0 until other.size) add(other[i])
    }
}

/** [block] run while holding [lock] if [concurrent], as it is otherwise */
internal inline fun <T> lockedIf(
    concurrent: Boolean,
    lock: Any,
    crossinline block: () -> T,
): T = if (concurrent) locked(lock) { block() } else block()
