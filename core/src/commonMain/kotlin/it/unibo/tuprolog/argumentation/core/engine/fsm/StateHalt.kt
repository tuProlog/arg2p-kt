// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0): see README.md in this package
package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.exception.ResolutionException

/**
 * "Halt": the one true sink of the state machine, reached only via an uncaught exception (no `catch/3` matched
 * it all the way up to the root context, or a timeout occurred). Unlike `StateEnd`, there is no transition back
 * out of it -- resolution truly stops here, with [Solution.halt] wrapping [exception].
 */
data class StateHalt(
    override val exception: ResolutionException,
    override val context: PrologExecutionContext,
) : AbstractEndState(Solution.halt(context.query, exception), context),
    ExceptionalState {
    override fun clone(context: PrologExecutionContext): StateHalt = copy(exception = exception, context = context)
}
