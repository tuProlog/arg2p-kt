// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0): see README.md in this package
package it.unibo.tuprolog.argumentation.core.engine

import it.unibo.tuprolog.argumentation.core.engine.fsm.State
import it.unibo.tuprolog.solve.Solution

/**
 * Drives the `:solve-prolog` finite-state machine, one [State.next] step at a time, from [state] on, and turns every
 * visited end-state (see [it.unibo.tuprolog.argumentation.core.engine.fsm.EndState]) into an emitted [Solution]:
 * the piece [AbstractPrologSolver.solveImpl] wraps into a lazy [Sequence]. [hasNext] answers whether the current
 * [state] is (or can, via backtracking, resume being) an end-state, and [next] steps the machine forward until it
 * reaches one. Because this is a genuine [Iterator] over an explicit state value rather than host-language recursion,
 * resolution can be driven incrementally, one solution or even one transition at a time, without unwinding a call
 * stack; [onStateTransition] is told every transition (its source, its destination and its index) as it happens.
 */
internal class SolutionIterator(
    state: State,
    private val onStateTransition: (State, State, Long) -> Unit,
) : Iterator<Solution> {
    /** The state the machine is currently in (before the next call to [next] advances it). */
    var state: State = state
        private set

    /** How many transitions the state machine has performed so far. */
    var step: Long = 0
        private set

    override fun hasNext(): Boolean = state.let { !it.isEndState || it.castToEndState().hasOpenAlternatives }

    override fun next(): Solution {
        do {
            val previousState = state
            state = state.next()
            onStateTransition(previousState, state, ++step)
        } while (!state.isEndState)
        return state.castToEndState().solution
    }
}
