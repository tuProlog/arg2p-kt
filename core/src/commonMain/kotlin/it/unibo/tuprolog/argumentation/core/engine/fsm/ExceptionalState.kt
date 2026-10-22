// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0): see README.md in this package
package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.solve.exception.ResolutionException
import kotlin.js.JsName

/**
 * A [State] reached because of a [ResolutionException] -- either `StateException` (still looking for an
 * enclosing `catch/3`) or `StateHalt` (no handler was found, or the exception reached the root context).
 */
interface ExceptionalState : State {
    /** The exception that led to this state. */
    @JsName("exception")
    val exception: ResolutionException
}
