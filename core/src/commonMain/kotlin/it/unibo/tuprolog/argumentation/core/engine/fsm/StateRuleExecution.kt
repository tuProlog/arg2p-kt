// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0): see README.md in this package
package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.argumentation.core.libs.basic.Arg2pUnificator
import it.unibo.tuprolog.core.Rule
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
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
     * arg2p: the variables of the rule are renamed apart from the goal (as the clause is resolved, see ClauseResolution,
     * with the unificator of arg2p), so they are only bound by the unifier with the goal, and only occur in the head
     * and the body: the body is built with the unifier, and only the bindings of the variables of the goal
     * are added to the substitution of the resolution (which they would only make larger, and as costly to extend at
     * each step): when there are none, the substitution stays as it is. The head comes first in the unification, so
     * that a variable of the rule meeting one of the goal is bound to it rather than the other way round: a goal
     * passing its variables to a rule (as most do) adds no binding (with member/2 over a list, the substitution grew
     * by one binding for each element, and enumerating the list was cubic in its length).
     */
    override fun computeNext(): State {
        val rule = context.rules.current!!
        val goal = context.currentGoal!!
        val resolution =
            if (context.unificator === Arg2pUnificator) {
                ClauseResolution.of(rule, goal)
            } else {
                resolveRenamed(rule.freshCopy(), goal)
            }
        return when (resolution) {
            null -> failureState
            else -> {
                val (goalBindings, subGoals) = resolution
                val newSubstitution =
                    if (goalBindings.isEmpty()) context.substitution else (context.substitution + goalBindings).castToUnifier()
                StateGoalSelection(
                    context.copy(
                        goals = subGoals.toGoals(),
                        rules = Cursor.empty(),
                        substitution = newSubstitution,
                        step = nextStep(),
                    ),
                )
            }
        }
    }

    // The resolution with a renamed rule, through the unificator of the context
    private fun resolveRenamed(
        rule: Rule,
        goal: Term,
    ): Pair<Substitution.Unifier, Term>? {
        val substitution = context.unificator.mgu(rule.head, goal)
        if (!substitution.isSuccess) return null
        val unifier = substitution.castToUnifier()
        val ruleVariables = HashSet<Var>().also { rule.head.collectVariables(it) }
        val goalBindings = unifier.filter { (variable, _) -> variable !in ruleVariables }.castToUnifier()
        return goalBindings to rule.prepareForExecution(unifier).body[unifier]
    }

    override fun clone(context: PrologExecutionContext): StateRuleExecution = copy(context = context)
}

// The variables of a term, collected by a plain recursion (Term.variables nests a lazy sequence for each argument),
// skipping its ground parts (whose groundness is known: lists build it, other structures compute it once)
internal fun Term.collectVariables(into: MutableSet<Var>) {
    when (this) {
        is Var -> into.add(this)
        is Struct -> if (!isGround) for (i in 0 until arity) this[i].collectVariables(into)
        else -> Unit
    }
}
