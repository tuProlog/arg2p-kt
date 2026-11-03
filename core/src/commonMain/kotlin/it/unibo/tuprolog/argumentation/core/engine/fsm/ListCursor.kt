package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.utils.Cursor

/**
 * The items of a list from one of them on, as a cursor (arg2p): the one at the end is the empty cursor, so that one
 * of these always has a current item. The cursors of tuProlog over a sequence chain, for each item advanced to, a
 * lazy delegate and the cursor it builds (several objects, synchronized): the goals of a frame and the clauses
 * selected for a goal are known when the cursor is built, and read from their list.
 */
internal class ListCursor<T : Any> private constructor(
    private val items: List<T>,
    private val index: Int,
) : Cursor<T> {
    override val current: T
        get() = items[index]

    // Built the first time it is read: the cursor is read before being advanced to, and once more when it is
    private var nextValue: Cursor<out T>? = null

    override val next: Cursor<out T>
        get() = nextValue ?: (if (index + 1 < items.size) ListCursor(items, index + 1) else Cursor.empty()).also { nextValue = it }

    // As for the cursors of tuProlog, whether there is a current item
    override val hasNext: Boolean
        get() = true

    override val isOver: Boolean
        get() = false

    override val isLazy: Boolean
        get() = false

    override fun <R> map(mapper: (T) -> R): Cursor<out R> = Cursor.of(asSequence().map(mapper))

    override fun iterator(): Iterator<T> =
        object : Iterator<T> {
            private var cursor: Cursor<out T> = this@ListCursor

            override fun hasNext(): Boolean = !cursor.isOver

            override fun next(): T = cursor.current!!.also { cursor = cursor.next }
        }

    override fun toString(): String = if (index + 1 < items.size) "[${items[index]}, ...]" else "[${items[index]}]"

    companion object {
        /** The cursor over [items], from the first one: empty when there are none */
        fun <T : Any> of(items: List<T>): Cursor<out T> = if (items.isEmpty()) Cursor.empty() else ListCursor(items, 0)
    }
}
