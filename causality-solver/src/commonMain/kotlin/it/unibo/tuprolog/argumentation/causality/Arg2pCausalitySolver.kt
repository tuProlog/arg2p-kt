package it.unibo.tuprolog.argumentation.causality

import it.unibo.tuprolog.argumentation.causality.libs.CausalitySolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.libs.basic.Cache

fun Arg2pSolver.Companion.causality() =
    of(
        listOf(CausalitySolver(), Cache()),
        emptyList(),
    )
