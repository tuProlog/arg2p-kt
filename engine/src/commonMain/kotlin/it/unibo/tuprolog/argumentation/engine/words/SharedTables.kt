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

    /** The items of [other] added, in their order: the table grown once (a table is copied for each solver), with room left */
    fun addAll(other: GrowOnlyTable<T>) {
        val needed = size + other.size
        if (needed > items.size) {
            var capacity = items.size
            while (capacity < needed + (needed shr 2)) capacity *= 2
            items = items.copyOf(capacity)
        }
        other.items.copyInto(items, size, 0, other.size)
        size = needed
    }
}

/** [block] run while holding [lock] if [concurrent], as it is otherwise */
internal inline fun <T> lockedIf(
    concurrent: Boolean,
    lock: Any,
    crossinline block: () -> T,
): T = if (concurrent) locked(lock) { block() } else block()
