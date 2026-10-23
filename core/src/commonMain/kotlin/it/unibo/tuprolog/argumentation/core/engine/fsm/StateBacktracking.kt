// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0): see README.md in this package
package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.solve.Solution

/**
 * "Backtracking", the central hub of the state machine: if [PrologExecutionContext.choicePoints] is empty (or
 * exhausted), moves to `StateEnd` emitting a negative solution; otherwise it walks the choice-point queue's
 * `pathToRoot` for the nearest choice point that still has an alternative, restores that saved execution-context
 * lineage (see [it.unibo.tuprolog.argumentation.core.engine.ChoicePointContext.backtrack]), and resumes at either
 * `StatePrimitiveExecution` or `StateRuleExecution`, depending on which kind of alternative was pending.
 */
data class StateBacktracking(
    override val context: PrologExecutionContext,
) : AbstractState(context) {
    override fun computeNext(): State {
        val choicePoints = context.choicePoints
        return if (choicePoints.let { it === null || !it.hasOpenAlternatives }) {
            StateEnd(
                solution = Solution.no(context.query),
                context = context.copy(step = nextStep()),
            )
        } else {
            val choicePointContext = choicePoints!!.pathToRoot.first { it.alternatives.hasNext }
            // arg2p: backtracking into a primitive computes its next answers, which may raise errors
            try {
                val nextContext = choicePointContext.backtrack(context)
                if (nextContext.primitives.hasNext) {
                    StatePrimitiveExecution(nextContext)
                } else {
                    StateRuleExecution(nextContext)
                }
            } catch (exception: Exception) {
                val executionContext = choicePointContext.executionContext ?: context
                StateException(
                    exception.asResolutionException(executionContext).updateLastContext(executionContext.skipThrow()),
                    context.copy(step = nextStep()),
                )
            }
        }
    }
}
