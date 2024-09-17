package it.unibo.tuprolog.argumentation.actor

import it.unibo.tuprolog.argumentation.actor.libs.ActorSolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.libs.basic.Cache

fun Arg2pSolver.Companion.parallel() =
    of(
        listOf(ActorSolver(), Cache()),
        emptyList(),
    )
