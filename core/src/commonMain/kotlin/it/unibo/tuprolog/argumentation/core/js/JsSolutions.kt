@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.core.js

import it.unibo.tuprolog.solve.Solution
import kotlin.js.JsExport

/** The solutions of a goal, computed one at a time as they are asked for */
@JsExport
class JsSolutions internal constructor(
    private val solutions: Iterator<Solution>,
) {
    fun hasNext(): Boolean = solutions.hasNext()

    fun next(): JsSolution = JsSolution(solutions.next())
}
