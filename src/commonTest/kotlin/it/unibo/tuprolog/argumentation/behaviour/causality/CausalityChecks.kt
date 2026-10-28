package it.unibo.tuprolog.argumentation.behaviour.causality

import it.unibo.tuprolog.argumentation.Arg2p
import kotlin.test.assertEquals

// The causality tests of a solver (see Arg2p.causality): each is expected to hold, or not (outcome)

fun isNessOriginalIntervention(
    solver: Arg2p,
    intervention: String,
    effect: String,
    outcome: Boolean = true,
) = assertEquals(
    outcome,
    solver.causality.nessOriginalIntervention(intervention, effect),
    "ness_original_intervention($intervention, $effect)",
)

fun isNessOriginalCause(
    solver: Arg2p,
    cause: String,
    effect: String,
    outcome: Boolean = true,
) = assertEquals(
    outcome,
    solver.causality.nessOriginal(cause, effect) != null,
    "ness_original(_, $cause, $effect)",
)

fun isNessIntervention(
    solver: Arg2p,
    intervention: String,
    effect: String,
    outcome: Boolean = true,
) = assertEquals(
    outcome,
    solver.causality.nessIntervention(intervention, effect),
    "ness_intervention($intervention, $effect)",
)

fun isNessCause(
    solver: Arg2p,
    cause: String,
    effect: String,
    outcome: Boolean = true,
) = assertEquals(
    outcome,
    solver.causality.ness(cause, effect) != null,
    "ness(_, $cause, $effect)",
)

fun isButForCause(
    solver: Arg2p,
    cause: String,
    effect: String,
    outcome: Boolean = true,
) = assertEquals(
    outcome,
    solver.causality.butFor(cause, effect),
    "but_for($cause, $effect)",
)
