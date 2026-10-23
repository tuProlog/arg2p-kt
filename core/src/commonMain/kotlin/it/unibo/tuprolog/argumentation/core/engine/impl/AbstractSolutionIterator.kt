// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0): see README.md in this package
package it.unibo.tuprolog.argumentation.core.engine.impl

import it.unibo.tuprolog.argumentation.core.engine.SolutionIterator
import it.unibo.tuprolog.argumentation.core.engine.fsm.State
import it.unibo.tuprolog.solve.Solution

internal abstract class AbstractSolutionIterator(
    state: State,
) : SolutionIterator {
    final override var state: State = state
        private set

    final override var step: Long = 0
        private set

    final override fun hasNext(): Boolean = state.let { !it.isEndState || it.castToEndState().hasOpenAlternatives }

    final override fun next(): Solution {
        do {
            val previousState = state
            state = computeNextState(state, ++step)
            onStateTransition(previousState, state, step)
        } while (!state.isEndState)
        return state.castToEndState().solution
    }

    protected abstract fun computeNextState(
        state: State,
        step: Long,
    ): State
}
