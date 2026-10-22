// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0): see README.md in this package
package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.solve.Solution

/**
 * Common base of the two terminal [EndState]s (`StateEnd` and `StateHalt`). By default there is no further
 * transition from here ([computeNext] throws); `StateEnd` overrides this to loop back into `StateBacktracking`
 * when [PrologExecutionContext.hasOpenAlternatives] is true, which is what makes it a *resumable* terminal,
 * unlike `StateHalt`, the one true sink of the machine.
 */
abstract class AbstractEndState(
    override val solution: Solution,
    override val context: PrologExecutionContext,
) : AbstractState(context),
    EndState {
    override fun computeNext(): State = throw NoSuchElementException()
}
