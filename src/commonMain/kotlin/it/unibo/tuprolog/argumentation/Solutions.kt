@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.argumentation.core.model.terms.TermValues
import kotlin.js.JsExport
import kotlin.js.JsName
import it.unibo.tuprolog.solve.Solution as LogicSolution

/**
 * The solutions of a goal, computed one at a time as they are asked for: with [hasNext] and [next], or iterated in
 * Kotlin
 */
@JsExport
class Solutions internal constructor(
    private val solutions: Iterator<LogicSolution>,
) {
    fun hasNext(): Boolean = solutions.hasNext()

    fun next(): Solution = Solution(solutions.next())

    @JsExport.Ignore
    operator fun iterator(): Iterator<Solution> =
        object : Iterator<Solution> {
            override fun hasNext() = this@Solutions.hasNext()

            override fun next() = this@Solutions.next()
        }

    /** The solutions left, at most [limit] of them */
    @JsExport.Ignore
    fun toList(limit: Int = Int.MAX_VALUE): List<Solution> = iterator().asSequence().take(limit).toList()

    /** The solutions left, at most [limit] of them (an array in JavaScript) */
    @JsName("toArray")
    fun toArray(limit: Int = Int.MAX_VALUE): Array<Solution> = toList(limit).toTypedArray()
}

/** A solution of a goal: yes, no or halt (an error), with the values a yes solution binds to the variables of the goal */
@JsExport
class Solution internal constructor(
    private val solution: LogicSolution,
) {
    val isYes: Boolean get() = solution.isYes

    val isNo: Boolean get() = solution.isNo

    val isHalt: Boolean get() = solution.isHalt

    /** The goal, as a yes solution solves it (its variables bound), otherwise as it was asked, as Prolog text */
    val query: String get() = TermValues.format(solution.solvedQuery ?: solution.query)

    /** The error of a halt solution */
    val error: String? get() = solution.exception?.let { it.message ?: it.toString() }

    /** The variables of the goal a yes solution binds */
    @JsExport.Ignore
    @JsName("variablesList")
    val variables: List<String>
        get() = if (solution.isYes) solution.substitution.keys.map { it.name } else emptyList()

    /** The variables of the goal a yes solution binds (an array in JavaScript) */
    @JsName("variables")
    val variableArray: Array<String> get() = variables.toTypedArray()

    /** The values a yes solution binds to the variables of the goal, by their names */
    @JsExport.Ignore
    val bindings: Map<String, Value>
        get() =
            if (solution.isYes) {
                solution.substitution.entries.associate { (variable, value) ->
                    variable.name to TermValues.of(value)
                }
            } else {
                emptyMap()
            }

    /** The value a yes solution binds to the variable of the goal named [name] */
    fun value(name: String): Value? = if (solution.isYes) solution.substitution.getByName(name)?.let { TermValues.of(it) } else null

    /** The value a yes solution binds to the variable of the goal named [name], as Prolog text */
    fun text(name: String): String? = value(name)?.toString()

    override fun toString(): String =
        when {
            isYes -> "yes: $query"
            isNo -> "no: $query"
            else -> "halt: $error"
        }
}
