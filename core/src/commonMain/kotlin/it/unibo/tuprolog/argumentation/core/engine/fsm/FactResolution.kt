package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.engine.ClauseObservers
import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Struct

/**
 * A goal resolved in a single step when the clauses selected for it are a single fact (arg2p): in the frame of the
 * goal, as a frame of its own would resolve it, since the fact has no body to run and, alone, leaves no choice point
 * behind. Reading a part of a term through a module of accessors (`argument::claim(A, C)`) then costs a unification,
 * rather than a frame, a choice point and the exit from the frame.
 */
internal object FactResolution {
    /**
     * The state following the resolution of [goal], the current goal of [context] with its substitution applied,
     * with [clauses], the clauses selected for it, as step [step]; null unless they are a single fact
     */
    fun of(
        context: PrologExecutionContext,
        goal: Struct,
        clauses: List<Clause>,
        step: Long,
    ): State? {
        val fact = clauses.singleOrNull()?.takeIf { it.isFact } ?: return null
        val resolution =
            ClauseResolver.of(context.unificator).resolve(fact.castToRule(), goal)
                ?: return StateBacktracking(context.copy(step = step))
        ClauseObservers.resolved(fact)
        return StateGoalSelection(
            context.copy(goals = context.goals.next, substitution = resolution.extend(context.substitution), step = step),
        )
    }
}
