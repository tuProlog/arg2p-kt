// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0): see README.md in this package
package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.solve.TimeDuration
import it.unibo.tuprolog.solve.TimeInstant
import it.unibo.tuprolog.solve.currentTimeInstant
import it.unibo.tuprolog.solve.exception.ResolutionException
import it.unibo.tuprolog.solve.exception.TimeOutException

/**
 * Common base of every `:solve-prolog` [State]: wraps [computeNext] with a shared timeout check, so no
 * concrete state has to implement `SolveOptions.timeout` handling itself.
 *
 * [next] first compares [executionTime] against [PrologExecutionContext.startTime]/[PrologExecutionContext.maxDuration];
 * if the budget is exceeded it short-circuits straight to an [EndState] (`StateHalt`) carrying a [TimeOutException],
 * bypassing any pending `catch/3` (resource exhaustion is treated as unrecoverable), and so it does with a
 * [ResolutionException] telling so if the thread of the resolution was interrupted (arg2p: on the JVM, its caller
 * stopping it, see [resolutionInterrupted]);
 * otherwise it delegates to [computeNext], the actual per-state transition logic.
 */
abstract class AbstractState(
    override val context: PrologExecutionContext,
) : State {
    /**
     * The (lazily captured) instant this state is being evaluated at, used for the timeout check in [next] (arg2p:
     * states are confined to the thread of their resolution, so the lazy value needs no synchronization).
     */
    protected val executionTime: TimeInstant
        get() = executionTimeValue ?: currentTime().also { executionTimeValue = it }

    // Kept in a field rather than a lazy delegate, which each state allocated with its initializer
    private var executionTimeValue: TimeInstant? = null

    /**
     * Whether [executionTime] is past [PrologExecutionContext.startTime] + [PrologExecutionContext.maxDuration]
     * (arg2p: without a time limit, the clock is not read at each step).
     */
    protected open val isTimeout: Boolean
        get() = context.maxDuration != TimeDuration.MAX_VALUE && executionTime - context.startTime > context.maxDuration

    override fun next(): State =
        when {
            isTimeout -> halt(TimeOutException(exceededDuration = context.maxDuration, context = context))
            resolutionInterrupted() -> halt(ResolutionException(INTERRUPTED, context = context))
            else -> computeNext()
        }

    // The resolution stopped by [exception], uncaught
    private fun halt(exception: ResolutionException): State = StateHalt(exception, context.copy(step = nextStep()))

    /** The actual, state-specific transition logic, invoked by [next] once the timeout check has passed. */
    protected abstract fun computeNext(): State

    protected fun currentTime(): TimeInstant = currentTimeInstant()

    protected fun nextStep(): Long = context.step + 1

    private companion object {
        // The message of the halt of a resolution its caller interrupted
        const val INTERRUPTED = "The resolution was interrupted by its caller"
    }
}

/**
 * Whether the resolution running on this thread was interrupted by its caller, which stops it: on the JVM, the
 * interruption of its thread (Thread.interrupt, Future.cancel(true)); never in JavaScript
 */
internal expect fun resolutionInterrupted(): Boolean
