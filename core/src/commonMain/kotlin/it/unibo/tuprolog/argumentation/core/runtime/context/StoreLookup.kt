package it.unibo.tuprolog.argumentation.core.runtime.context

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term

/** The goal looking up a goal in a store of clauses (as `context_check(Branch, Goal)` or `cache_check(Goal)`) */
internal fun interface StoreLookup {
    fun of(goal: Term): Struct
}
