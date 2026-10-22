// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0): see README.md in this package
package it.unibo.tuprolog.argumentation.core.engine.impl

import it.unibo.tuprolog.argumentation.core.engine.MutableSolutionIterator
import it.unibo.tuprolog.argumentation.core.engine.fsm.State

internal class HijackableSolutionIterator(
    state: State,
    private val hijackStateTransitionCallback: (State, State, Long) -> State,
    private val onStateTransitionCallback: (State, State, Long) -> Unit,
) : AbstractSolutionIterator(state),
    MutableSolutionIterator {
    override fun computeNextState(
        state: State,
        step: Long,
    ): State = hijackStateTransition(state, state.next(), step)

    override fun hijackStateTransition(
        source: State,
        destination: State,
        index: Long,
    ): State = hijackStateTransitionCallback(source, destination, index)

    override fun onStateTransition(
        source: State,
        destination: State,
        index: Long,
    ) = onStateTransitionCallback(source, destination, index)
}
