// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0): see README.md in this package
package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.core.prepareForExecution
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
     * arg2p: the rule is a fresh copy, whose variables are only bound by the unifier with the goal, and only occur in
     * the head and the body: the body is built with the unifier, and only the bindings of the variables of the goal
     * are added to the substitution of the resolution (which they would only make larger, and as costly to extend at
     * each step): when there are none, the substitution stays as it is.
     */
    override fun computeNext(): State {
        val rule = context.rules.current!!
        val substitution = with(context) { unificator.mgu(currentGoal!!, rule.head) }
        return when {
            substitution.isSuccess -> {
                val unifier = substitution.castToUnifier()
                val ruleVariables = rule.head.variables.toSet()
                val goalBindings = unifier.filter { (variable, _) -> variable !in ruleVariables }
                val newSubstitution =
                    if (goalBindings.isEmpty()) context.substitution else (context.substitution + goalBindings).castToUnifier()
                val subGoals =
                    rule
                        .prepareForExecution(unifier)
                        .body[unifier]

                StateGoalSelection(
                    context.copy(
                        goals = subGoals.toGoals(),
                        rules = Cursor.empty(),
                        substitution = newSubstitution,
                        step = nextStep(),
                    ),
                )
            }
            else -> failureState
        }
    }

    override fun clone(context: PrologExecutionContext): StateRuleExecution = copy(context = context)
}
