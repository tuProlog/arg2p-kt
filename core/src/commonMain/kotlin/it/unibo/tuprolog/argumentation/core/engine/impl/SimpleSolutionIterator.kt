// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0): see README.md in this package
package it.unibo.tuprolog.argumentation.core.engine.impl

import it.unibo.tuprolog.argumentation.core.engine.fsm.State

internal class SimpleSolutionIterator(
    state: State,
    private val onStateTransitionCallback: (State, State, Long) -> Unit,
) : AbstractSolutionIterator(state) {
    override fun computeNextState(
        state: State,
        step: Long,
    ): State = state.next()

    override fun onStateTransition(
        source: State,
        destination: State,
        index: Long,
    ) = onStateTransitionCallback(source, destination, index)
}
