// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0): see README.md in this package
package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.argumentation.core.engine.RelevantGoals
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.flags.TrackVariables

/**
 * "Goal Selection", the entry point of every resolution step: decides what to do with
 * [PrologExecutionContext.goals]. Three cases: no goals left and no parent context -> emit the current
 * substitution as a solution and move to `StateEnd`; no goals left but a parent exists -> pop the stack, carrying
 * the child's substitution (filtered down to variables the parent still cares about, via
 * [PrologExecutionContext.isVariableInteresting]) into the parent, and loop back into `StateGoalSelection` for
 * the parent's remaining goals; goals remain -> move to `StatePrimitiveSelection` (after recording the current
 * goal's variables as relevant, if `TrackVariables` is `ON`).
 */
data class StateGoalSelection(
    override val context: PrologExecutionContext,
) : AbstractState(context) {
    @Suppress("ktlint:standard:discouraged-comment-location")
    override fun computeNext(): State =
        if (context.goals.isOver) {
            if (context.isRoot) {
                StateEnd(
                    Solution.yes(context.query, context.substitution),
                    context.copy(step = nextStep()),
                )
            } else {
                // The bindings of the variables of no more interest are dropped once the substitution has doubled
                // since it was last trimmed (arg2p: trimming it at every frame left made each step cost as much as
                // all the variables in the frames above): keeping them a little longer does not change anything else
                val trim = context.substitution.size > maxOf(MIN_TRIMMED_SIZE, 2 * context.trimmedSize)
                val substitution =
                    if (trim) {
                        context.substitution.filter { (it, _) -> context.isVariableInteresting(it) }.castToUnifier()
                    } else {
                        context.substitution
                    }
                StateGoalSelection(
                    context.parent!!.let {
                        it.copy(
                            choicePoints = context.choicePoints,
                            flags = context.flags,
                            dynamicKb = context.dynamicKb,
                            staticKb = context.staticKb,
                            substitution = substitution,
                            trimmedSize = if (trim) substitution.size else context.trimmedSize,
                            goals = it.goals.next, // go on with parent's goals
                            procedure = it.procedure,
                            step = nextStep(),
                            startTime = context.startTime,
                            operators = context.operators,
                            inputChannels = context.inputChannels,
                            outputChannels = context.outputChannels,
                            libraries = context.libraries,
                            customData = context.customData,
                        )
                    },
                )
            }
        } else if (context.flags[TrackVariables] == TrackVariables.ON) {
            StatePrimitiveSelection(
                context.copy(
                    step = nextStep(),
                    relevantGoals = RelevantGoals(context.goals.current!!, context.relevantGoals),
                ),
            )
        } else {
            StatePrimitiveSelection(
                context.copy(step = nextStep()),
            )
        }

    override fun clone(context: PrologExecutionContext): StateGoalSelection = copy(context = context)

    private companion object {
        const val MIN_TRIMMED_SIZE = 32
    }
}
