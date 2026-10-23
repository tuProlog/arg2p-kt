// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0): see README.md in this package
package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import kotlin.js.JsName

/**
 * One location of the `:solve-prolog` finite-state machine: an explicit, inspectable representation of "where
 * resolution currently is", carrying the [PrologExecutionContext] it operates on.
 *
 * The nine concrete states -- `StateGoalSelection`, `StatePrimitiveSelection`, `StatePrimitiveExecution`,
 * `StateRuleSelection`, `StateRuleExecution`, `StateBacktracking`, `StateException`, `StateEnd` and `StateHalt`
 * (plus a bootstrap `StateInit`) -- correspond one-to-one to the nine "locations" of the formal model in
 * Ciatto's 2021 paper "Formal Modelling of a Prolog Solver as a State Machine", which traces the approach back
 * to Piancastelli's original state-machine design for tuProlog; see the project's "state-machine" explanation
 * page for the full walk-through of every transition and why cut/catch/backtracking are implemented the way
 * they are. [next] computes the successor state as a pure function of [context]; a [it.unibo.tuprolog.argumentation.core.engine.SolutionIterator]
 * drives this loop and turns every visit to an [EndState] ([isEndState]) into a [it.unibo.tuprolog.solve.Solution].
 *
 * Modelling a resolution step as a value (rather than as a stack frame of host-language recursion) is what
 * allows a step to be paused, resumed, inspected or hijacked (as the `MutableSolutionIterator` of tuProlog's classic solver)
 * without unwinding or rebuilding a JVM/JS call stack, and sidesteps host stack-depth limits for deeply
 * recursive Prolog programs, since the "call stack" here is the [PrologExecutionContext] parent chain --
 * ordinary heap data, not native stack frames.
 */
interface State {
    /** Whether this state is a terminal one for the current resolution step, i.e. an [EndState]. */
    @JsName("isEndState")
    val isEndState: Boolean
        get() = false

    /** This state as an [EndState], or `null` if [isEndState] is `false`. */
    @JsName("asEndState")
    fun asEndState(): EndState? = null

    /** This state as an [EndState].
     * @throws ClassCastException if [isEndState] is `false`.
     */
    @JsName("castToEndState")
    fun castToEndState(): EndState = asEndState() ?: throw ClassCastException("Cannot cast $this to ${EndState::class.simpleName}")

    /** The execution context this state operates on. */
    @JsName("context")
    val context: PrologExecutionContext

    /** Computes the state the machine transitions into from here -- a pure function of [context]. */
    @JsName("next")
    fun next(): State
}
