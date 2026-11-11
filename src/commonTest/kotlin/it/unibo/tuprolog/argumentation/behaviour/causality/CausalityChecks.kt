package it.unibo.tuprolog.argumentation.behaviour.causality

import it.unibo.tuprolog.argumentation.Arg2p
import kotlin.test.assertEquals
import kotlin.test.fail

// The causality tests of a solver, goals of its causality module: each is expected to hold, or not (outcome)

private fun check(
    solver: Arg2p,
    goal: String,
    outcome: Boolean,
) {
    val solution = solver.solve("causality::$goal").next()
    if (solution.isHalt) fail("$goal: ${solution.error}")
    assertEquals(outcome, solution.isYes, goal)
}

fun isNessOriginalIntervention(
    solver: Arg2p,
    intervention: String,
    effect: String,
    outcome: Boolean = true,
) = check(solver, "nessOriginalIntervention($intervention, $effect)", outcome)

fun isNessOriginalCause(
    solver: Arg2p,
    cause: String,
    effect: String,
    outcome: Boolean = true,
) = check(solver, "nessOriginal(_, $cause, $effect)", outcome)

fun isNessIntervention(
    solver: Arg2p,
    intervention: String,
    effect: String,
    outcome: Boolean = true,
) = check(solver, "nessIntervention($intervention, $effect)", outcome)

fun isNessCause(
    solver: Arg2p,
    cause: String,
    effect: String,
    outcome: Boolean = true,
) = check(solver, "ness(_, $cause, $effect)", outcome)

fun isButForCause(
    solver: Arg2p,
    cause: String,
    effect: String,
    outcome: Boolean = true,
) = check(solver, "butFor($cause, $effect)", outcome)
