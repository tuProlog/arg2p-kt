package it.unibo.tuprolog.argumentation.actor

import it.unibo.tuprolog.argumentation.actor.actors.ResponseConsumer
import it.unibo.tuprolog.argumentation.actor.libs.ActorSolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import kotlin.jvm.JvmOverloads
import kotlin.time.Duration

/**
 * The libraries of a solver evaluating its theory on a cluster of actors (module `parallel`): the node of the solver
 * binds to [hostname], the address the other nodes reach it at (the local one by default, for a cluster on a single
 * machine), and waits [timeout] at most for the answer to a query, an error past it
 */
@JvmOverloads
fun Arg2pSolver.Companion.parallel(
    hostname: String = ClusterInitializer.LOCAL_HOSTNAME,
    timeout: Duration = ResponseConsumer.DEFAULT_TIMEOUT,
) = of(
    listOf(ActorSolver(hostname, timeout)),
    emptyList(),
)
