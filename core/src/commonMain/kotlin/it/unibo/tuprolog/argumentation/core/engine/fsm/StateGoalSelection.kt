// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0): see README.md in this package
package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.Solution

/**
 * "Goal Selection", the entry point of every resolution step: decides what to do with
 * [PrologExecutionContext.goals]. Three cases: no goals left and no parent context -> emit the current
 * substitution as a solution and move to `StateEnd`; no goals left but a parent exists -> pop the stack, carrying
 * the child's substitution (filtered down to variables the parent still cares about, via
 * [PrologExecutionContext.isVariableInteresting]) into the parent, and loop back into `StateGoalSelection` for
 * the parent's remaining goals; goals remain -> move to `StatePrimitiveSelection`.
 */
data class StateGoalSelection(
    override val context: PrologExecutionContext,
) : AbstractState(context) {
    @Suppress("ktlint:standard:discouraged-comment-location")
    override fun computeNext(): State =
        if (context.goals.isOver) {
            if (context.isRoot) {
                // arg2p: the bindings of the variables of the query alone (see Bindings.retaining): the solver cleans
                // the solution up to them anyway, copying its substitution (thrice) however large
                val queryVariables = HashSet<Var>().also { context.query.collectVariables(it) }
                StateEnd(
                    Solution.yes(context.query, context.bindings.retaining(queryVariables).unifier),
                    context.copy(step = nextStep()),
                )
            } else {
                // The bindings of the variables of no more interest are dropped once the substitution has doubled
                // since it was last trimmed (arg2p: trimming it at every frame left made each step cost as much as
                // all the variables in the frames above): keeping them a little longer does not change anything else
                val trim = context.bindings.size > maxOf(MIN_TRIMMED_SIZE, 2 * context.trimmedSize)
                val bindings =
                    if (trim) {
                        context.bindings.filter { context.isVariableInteresting(it) }
                    } else {
                        context.bindings
                    }
                StateGoalSelection(
                    context.parent!!.let {
                        it.copy(
                            choicePoints = context.choicePoints,
                            bindings = bindings,
                            trimmedSize = if (trim) bindings.size else context.trimmedSize,
                            goals = it.goals.next, // go on with parent's goals
                            step = nextStep(),
                            // The environment of the frame left (its side effects stay), with the libraries of
                            // the parent where the frame was an inlined call: its libraries only apply to its goal
                            environment =
                                if (context.inlined) {
                                    context.environment.carried(libraries = it.libraries, candidate = it.environment)
                                } else {
                                    context.environment
                                },
                        )
                    },
                )
            }
        } else {
            StatePrimitiveSelection(context.advanced())
        }

    private companion object {
        const val MIN_TRIMMED_SIZE = 32
    }
}
