package it.unibo.tuprolog.argumentation.actor

import it.unibo.tuprolog.argumentation.actor.libs.ActorSolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.libs.basic.Cache

fun arg2pParallelSolver() = Arg2pSolver.of(
    listOf(ActorSolver(), Cache()),
    emptyList()
)
