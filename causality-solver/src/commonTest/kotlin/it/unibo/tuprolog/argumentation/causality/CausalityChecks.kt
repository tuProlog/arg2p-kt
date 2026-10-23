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
    reset: Boolean = true,
) = check(solver, "ness_original_intervention", listOf(intervention, effect), outcome, reset)

fun isNessOriginalCause(
    solver: MutableSolver,
    cause: String,
    effect: String,
    outcome: Boolean = true,
    reset: Boolean = true,
) = check(solver, "ness_original", listOf(null, cause, effect), outcome, reset)

fun isNessIntervention(
    solver: MutableSolver,
    intervention: String,
    effect: String,
    outcome: Boolean = true,
    reset: Boolean = true,
) = check(solver, "ness_intervention", listOf(intervention, effect), outcome, reset)

fun isNessCause(
    solver: MutableSolver,
    cause: String,
    effect: String,
    outcome: Boolean = true,
    reset: Boolean = true,
) = check(solver, "ness", listOf(null, cause, effect), outcome, reset)

fun isButForCause(
    solver: MutableSolver,
    cause: String,
    effect: String,
    outcome: Boolean = true,
    reset: Boolean = true,
) = check(solver, "but_for", listOf(cause, effect), outcome, reset)

// The predicate on the arguments (null for a variable, the intervention found), after a reset of the context if reset
private fun check(
    solver: MutableSolver,
    predicate: String,
    arguments: List<String?>,
    outcome: Boolean,
    reset: Boolean,
) = arg2pScope {
    val goal = Struct.of(predicate, arguments.map { it?.let { Term.parse(it) } ?: X })
    assertEquals(outcome, solver.solve(if (reset) "context_reset" and goal else goal).first().isYes, "$goal")
}
