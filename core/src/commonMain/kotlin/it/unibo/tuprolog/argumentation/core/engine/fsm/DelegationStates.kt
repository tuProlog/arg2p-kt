package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.engine.Delegation
import it.unibo.tuprolog.argumentation.core.engine.Inlining
import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.argumentation.core.engine.Sourcing
import it.unibo.tuprolog.utils.Cursor
import it.unibo.tuprolog.utils.cursor

/**
 * How the engine solves a [it.unibo.tuprolog.argumentation.core.engine.DelegatedCall] (arg2p): in [frame], the frame
 * of the call, opaque to cut, with a choice point of its own (the one a cut in the delegated goal finds).
 */
internal object DelegationStates {
    fun of(
        delegation: Delegation,
        frame: PrologExecutionContext,
    ): State =
        when (delegation) {
            is Inlining -> inlined(delegation, frame)
            is Sourcing -> sourced(delegation, frame)
        }

    // The goal solved as any goal, against the libraries of the inlining
    private fun inlined(
        inlining: Inlining,
        frame: PrologExecutionContext,
    ): State =
        StateGoalSelection(
            frame
                .copy(goals = inlining.goal.toGoals(), libraries = inlining.libraries, inlined = true, scoped = true)
                .appendRulesAndChoicePoints(Cursor.empty()),
        )

    // The goal resolved with the clauses of the source, as with the clauses selected for a goal of the program
    private fun sourced(
        sourcing: Sourcing,
        frame: PrologExecutionContext,
    ): State {
        val rules = RuleCursor.of(sourcing.source.clausesFor(sourcing.goal, frame.unificator))
        return if (rules.isOver) {
            StateBacktracking(frame)
        } else {
            StateRuleExecution(frame.copy(goals = sequenceOf(sourcing.goal).cursor()).appendRulesAndChoicePoints(rules))
        }
    }
}
