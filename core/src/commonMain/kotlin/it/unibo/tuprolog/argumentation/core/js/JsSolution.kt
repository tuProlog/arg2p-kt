@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.core.js

import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.Solution
import kotlin.js.JsExport

/** A solution of a goal: yes, no or halt (an error), with the bindings of the variables of the goal */
@JsExport
class JsSolution internal constructor(
    /** The solution of tuProlog */
    val solution: Solution,
) {
    val isYes: Boolean
        get() = solution.isYes

    val isNo: Boolean
        get() = solution.isNo

    val isHalt: Boolean
        get() = solution.isHalt

    /** The goal, as a yes solution solves it (its variables bound), otherwise as it was asked, as text */
    val query: String
        get() = JsTerms.format(solution.solvedQuery ?: solution.query)

    /** The error of a halt solution, as text */
    val error: String?
        get() = solution.exception?.toString()

    /** The variables of the goal bound by a yes solution */
    val variables: Array<String>
        get() =
            if (solution.isYes) {
                solution.substitution.keys
                    .map { it.name }
                    .toTypedArray()
            } else {
                emptyArray()
            }

    /** The value of the variable of the goal named [name], if a yes solution binds it */
    fun valueOf(name: String): Term? = if (solution.isYes) solution.substitution.getByName(name) else null

    /** The value of the variable of the goal named [name] as text, if a yes solution binds it */
    fun formatted(name: String): String? = valueOf(name)?.let { JsTerms.format(it) }

    override fun toString(): String = solution.toString()
}
