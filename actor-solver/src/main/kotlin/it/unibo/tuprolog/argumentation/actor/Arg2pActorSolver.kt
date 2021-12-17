package it.unibo.tuprolog.argumentation.actor

import it.unibo.tuprolog.argumentation.actor.libs.ActorSolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.libs.basic.Cache
import it.unibo.tuprolog.argumentation.core.libs.basic.Context
import it.unibo.tuprolog.argumentation.core.libs.basic.EngineInterface
import it.unibo.tuprolog.argumentation.core.libs.language.RuleParser
import it.unibo.tuprolog.argumentation.core.libs.structured.StructuredMode
import it.unibo.tuprolog.argumentation.core.libs.utils.Debug
import it.unibo.tuprolog.argumentation.core.libs.utils.Utils

fun arg2pParallelSolver() = Arg2pSolver.of(
    listOf(EngineInterface, ActorSolver(), Context(), Cache()),
    listOf(
        Utils,
        Debug,
        RuleParser,
        StructuredMode
    )
)
