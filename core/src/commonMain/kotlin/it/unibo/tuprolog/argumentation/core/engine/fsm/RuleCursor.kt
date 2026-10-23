package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Rule
import it.unibo.tuprolog.utils.Cursor

/**
 * The clauses selected for a goal, from the one at [index], as stored: their variables are renamed as they are
 * resolved (see [ClauseResolver])
 */
internal class RuleCursor private constructor(
    private val clauses: List<Clause>,
    private val index: Int,
) : Cursor<Rule> {
    override val current: Rule? by lazy(LazyThreadSafetyMode.NONE) {
        if (index < clauses.size) clauses[index].castToRule() else null
    }

    override val next: Cursor<out Rule> by lazy(LazyThreadSafetyMode.NONE) {
        if (index + 1 < clauses.size) RuleCursor(clauses, index + 1) else Cursor.empty()
    }

    // As for the cursors of tuProlog, whether there is a current clause
    override val hasNext: Boolean
        get() = index < clauses.size

    override val isOver: Boolean
        get() = index >= clauses.size

    override val isLazy: Boolean
        get() = true

    override fun <R> map(mapper: (Rule) -> R): Cursor<out R> = Cursor.of(asSequence().map(mapper))

    override fun iterator(): Iterator<Rule> =
        object : Iterator<Rule> {
            private var cursor: Cursor<out Rule> = this@RuleCursor

            override fun hasNext(): Boolean = !cursor.isOver

            override fun next(): Rule = cursor.current!!.also { cursor = cursor.next }
        }

    override fun toString(): String = if (isOver) "[]" else "[${clauses[index]}, ...]"

    companion object {
        fun of(clauses: List<Clause>): Cursor<out Rule> = if (clauses.isEmpty()) Cursor.empty() else RuleCursor(clauses, 0)
    }
}
