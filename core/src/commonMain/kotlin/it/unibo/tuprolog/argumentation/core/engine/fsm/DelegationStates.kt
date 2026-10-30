package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.engine.Delegation
import it.unibo.tuprolog.argumentation.core.engine.Inlining
import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.argumentation.core.engine.Sourcing
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.solve.extractSignature
import it.unibo.tuprolog.utils.Cursor
import it.unibo.tuprolog.utils.cursor

/**
 * How the engine solves a [it.unibo.tuprolog.argumentation.core.engine.DelegatedCall] (arg2p), made in `caller`: in
 * `frame`, the frame of the call, opaque to cut, with a choice point of its own (the one a cut in the delegated goal
 * finds), or, for a goal resolved with a single fact, in `caller` at `step` (see [FactResolution]).
 */
internal object DelegationStates {
    fun of(
        delegation: Delegation,
        caller: PrologExecutionContext,
        frame: PrologExecutionContext,
        step: Long,
    ): State =
        when (delegation) {
            is Inlining -> singleFact(delegation, caller, step) ?: inlined(delegation, frame)
            is Sourcing -> sourced(delegation, frame)
        }

    // The goal solved as any goal, against the libraries of the inlining, seeing the theory of the user as it says
    private fun inlined(
        inlining: Inlining,
        frame: PrologExecutionContext,
    ): State =
        StateGoalSelection(
            frame
                .copy(
                    goals = inlining.goal.toGoals(),
                    libraries = inlining.libraries,
                    theoryVisible = inlining.theory,
                    inlined = true,
                    scoped = true,
                ).appendRulesAndChoicePoints(Cursor.empty()),
        )

    // A goal of a predicate of the program (neither a control construct nor a primitive), resolved with a single
    // fact against the libraries of the inlining: the libraries only apply to the goal, which leaves nothing to solve
    private fun singleFact(
        inlining: Inlining,
        caller: PrologExecutionContext,
        step: Long,
    ): State? {
        val goal = inlining.goal
        val solvedOtherwise =
            goal.isTruth ||
                (goal.isAtom && goal.castToAtom().value == "!") ||
                goal.functor in Clause.notableFunctors ||
                ControlConstructs.of(goal) != null ||
                inlining.libraries.hasPrimitive(goal.extractSignature())
        if (solvedOtherwise) return null
        val clauses = ProgramClauses.of(caller, inlining.libraries, inlining.theory).clausesFor(goal, caller.unificator)
        return FactResolution.of(caller, goal, clauses, step)
    }

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
