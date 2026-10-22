// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0): see README.md in this package
package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.solve.TimeDuration
import it.unibo.tuprolog.solve.TimeInstant
import it.unibo.tuprolog.solve.currentTimeInstant
import it.unibo.tuprolog.solve.exception.TimeOutException

/**
 * Common base of every `:solve-prolog` [State]: wraps [computeNext] with a shared timeout check, so no
 * concrete state has to implement `SolveOptions.timeout` handling itself.
 *
 * [next] first compares [executionTime] against [PrologExecutionContext.startTime]/[PrologExecutionContext.maxDuration];
 * if the budget is exceeded it short-circuits straight to an [EndState] (`StateHalt`) carrying a
 * [TimeOutException], bypassing any pending `catch/3` (resource exhaustion is treated as unrecoverable);
 * otherwise it delegates to [computeNext], the actual per-state transition logic.
 */
abstract class AbstractState(
    override val context: PrologExecutionContext,
) : State {
    /**
     * The (lazily captured) instant this state is being evaluated at, used for the timeout check in [next] (arg2p:
     * states are confined to the thread of their resolution, so the lazy value needs no synchronization).
     */
    protected val executionTime: TimeInstant by lazy(LazyThreadSafetyMode.NONE) {
        currentTime()
    }

    /**
     * Whether [executionTime] is past [PrologExecutionContext.startTime] + [PrologExecutionContext.maxDuration]
     * (arg2p: without a time limit, the clock is not read at each step).
     */
    protected open val isTimeout: Boolean
        get() = context.maxDuration != TimeDuration.MAX_VALUE && executionTime - context.startTime > context.maxDuration

    override fun next(): State =
        if (isTimeout) {
            StateHalt(
                TimeOutException(
                    exceededDuration = context.maxDuration,
                    context = context,
                ),
                context.copy(step = nextStep()),
            )
        } else {
            computeNext()
        }

    /** The actual, state-specific transition logic, invoked by [next] once the timeout check has passed. */
    protected abstract fun computeNext(): State

    protected fun currentTime(): TimeInstant = currentTimeInstant()

    protected fun nextStep(): Long = context.step + 1
}
