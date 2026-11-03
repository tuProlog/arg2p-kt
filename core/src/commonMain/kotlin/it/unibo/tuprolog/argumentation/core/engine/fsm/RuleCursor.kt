package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Rule
import it.unibo.tuprolog.utils.Cursor

/**
 * The clauses selected for a goal, as stored: their variables are renamed as they are resolved (see [ClauseResolver])
 */
internal object RuleCursor {
    /** The cursor over [clauses], all rules (the clauses selected for a goal have heads) */
    @Suppress("UNCHECKED_CAST")
    fun of(clauses: List<Clause>): Cursor<out Rule> = ListCursor.of(clauses as List<Rule>)
}
