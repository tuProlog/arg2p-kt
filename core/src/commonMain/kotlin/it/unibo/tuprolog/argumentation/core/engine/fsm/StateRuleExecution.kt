// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0): see README.md in this package
package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.utils.Cursor

/**
 * "Rule Execution": pops the first candidate [it.unibo.tuprolog.core.Rule] from [PrologExecutionContext.rules]
 * and unifies its head with the current goal. On success, the rule's body (with the goal's substitution already
 * applied) becomes the new goal stream and the machine moves back to `StateGoalSelection`; on failure, it moves
 * to `StateBacktracking`.
 */
data class StateRuleExecution(
    override val context: PrologExecutionContext,
) : AbstractState(context) {
    private val failureState: StateBacktracking
        get() = StateBacktracking(context.copy(rules = Cursor.empty(), step = nextStep()))

    /*
     * arg2p: the clause is resolved as the resolver for the unificator does (see ClauseResolver): its variables are
     * renamed apart from the goal, so they are only bound by the unifier with the goal, and only occur in its body.
     * Only the bindings of the variables of the goal are added to the substitution of the resolution (which they
     * would only make larger, and as costly to extend at each step): when there are none, the substitution stays as
     * it is. A variable of the rule meeting one of the goal is bound to it rather than the other way round: a goal
     * passing its variables to a rule (as most do) adds no binding (with member/2 over a list, the substitution grew
     * by one binding for each element, and enumerating the list was cubic in its length).
     */
    override fun computeNext(): State {
        val resolution = ClauseResolver.of(context.unificator).resolve(context.rules.current!!, context.currentGoal!!)
        return if (resolution == null) {
            failureState
        } else {
            val bindings = resolution.goalBindings
            StateGoalSelection(
                context.copy(
                    goals = resolution.body.toBodyGoals(),
                    rules = Cursor.empty(),
                    substitution = if (bindings.isEmpty()) context.substitution else (context.substitution + bindings).castToUnifier(),
                    step = nextStep(),
                ),
            )
        }
    }

    override fun clone(context: PrologExecutionContext): StateRuleExecution = copy(context = context)
}
