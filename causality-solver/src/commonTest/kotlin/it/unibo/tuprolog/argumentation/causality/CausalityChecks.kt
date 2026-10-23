package it.unibo.tuprolog.argumentation.causality

import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.MutableSolver
import kotlin.test.assertEquals

// The checks of the causality module on a solver: each is expected to hold, or not (outcome)

fun isNessOriginalIntervention(
    solver: MutableSolver,
    intervention: String,
    effect: String,
    outcome: Boolean = true,
) = check(solver, "ness_original_intervention", listOf(intervention, effect), outcome)

fun isNessOriginalCause(
    solver: MutableSolver,
    cause: String,
    effect: String,
    outcome: Boolean = true,
) = check(solver, "ness_original", listOf(null, cause, effect), outcome)

fun isNessIntervention(
    solver: MutableSolver,
    intervention: String,
    effect: String,
    outcome: Boolean = true,
) = check(solver, "ness_intervention", listOf(intervention, effect), outcome)

fun isNessCause(
    solver: MutableSolver,
    cause: String,
    effect: String,
    outcome: Boolean = true,
) = check(solver, "ness", listOf(null, cause, effect), outcome)

fun isButForCause(
    solver: MutableSolver,
    cause: String,
    effect: String,
    outcome: Boolean = true,
) = check(solver, "but_for", listOf(cause, effect), outcome)

// The predicate on the arguments (null for a variable, the intervention found), after a reset of the context
private fun check(
    solver: MutableSolver,
    predicate: String,
    arguments: List<String?>,
    outcome: Boolean,
) = arg2pScope {
    val goal = Struct.of(predicate, arguments.map { it?.let { Term.parse(it) } ?: X })
    assertEquals(outcome, solver.solve("context_reset" and goal).first().isYes, "$goal")
}
