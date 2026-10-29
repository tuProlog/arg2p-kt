package it.unibo.tuprolog.argumentation.actor.libs

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import it.unibo.tuprolog.argumentation.actor.ClusterException
import it.unibo.tuprolog.argumentation.actor.ClusterInitializer
import it.unibo.tuprolog.argumentation.actor.actors.ResponseConsumer
import it.unibo.tuprolog.argumentation.actor.message.Add
import it.unibo.tuprolog.argumentation.actor.message.KbMessage
import it.unibo.tuprolog.argumentation.actor.message.Reset
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import it.unibo.tuprolog.argumentation.core.modules.ArgLibrary
import it.unibo.tuprolog.argumentation.core.modules.Loadable
import it.unibo.tuprolog.argumentation.core.modules.graph.language.ParserModule
import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.SystemError
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Solve
import kotlin.time.Duration

/**
 * The actor solver (module `parallel`): the theory of the solver evaluated by a cluster of actors, which it joins
 * (join/1,2), loads (load/0), queries (solve/4), resets (reset/0) and leaves (leave/0). The node binds to [hostname],
 * the address the other nodes reach it at, and waits [timeout] at most for the answer to a query. Any of them but
 * join and leave, with no cluster joined (or after leaving it), raises a system error, as a query with no answer in
 * time does.
 */
internal class ActorSolver(
    private val hostname: String = ClusterInitializer.LOCAL_HOSTNAME,
    private val timeout: Duration = ResponseConsumer.DEFAULT_TIMEOUT,
) : ArgLibrary,
    Loadable {
    // The node of the cluster joined, if one is: its actor system, and the distributor of the knowledge base
    private var node: Pair<ActorSystem<KbMessage>, ActorRef<KbMessage>>? = null

    // The node of the cluster joined: a system error for [request] if none is
    private fun joined(request: Solve.Request<ExecutionContext>): Pair<ActorSystem<KbMessage>, ActorRef<KbMessage>> =
        node ?: throw error(request, "No cluster joined: join one first (join/1, join/2)")

    private fun error(
        request: Solve.Request<ExecutionContext>,
        message: String,
    ) = SystemError("parallel: $message", context = request.context, extraData = Atom.of(message))

    inner class ParallelSolve : PrimitiveWithSignature {
        override val signature = Signature("solve", 4)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val goal: Term = request.arguments[0]
            val (actorSystem, masterActor) = joined(request)

            val response =
                try {
                    ResponseConsumer.getResponse(goal.toString(), actorSystem, masterActor, timeout)
                } catch (e: ClusterException) {
                    throw error(request, e.message.orEmpty())
                }

            return arg2pScope {
                sequenceOf(
                    request.replyWith(
                        Substitution.Companion.of(
                            mapOf(
                                Pair(request.arguments[1] as Var, response.inArgs.map { it.claim }.toTerm()),
                                Pair(request.arguments[2] as Var, response.outArgs.map { it.claim }.toTerm()),
                                Pair(request.arguments[3] as Var, response.undArgs.map { it.claim }.toTerm()),
                            ),
                        ),
                    ),
                )
            }
        }
    }

    inner class ParallelLoad : PrimitiveWithSignature {
        override val signature = Signature("load", 0)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val (_, masterActor) = joined(request)
            // the facts by their head (the rules of arg2p are facts), the Prolog rules whole
            request.context.staticKb.forEach {
                masterActor.tell(Add((if (it.isFact) it.head else it).toString()))
            }
            return sequenceOf(request.replyWith(true))
        }
    }

    inner class ParallelInit : PrimitiveWithSignature {
        override val signature = Signature("join", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val port = request.arguments[0].toString()
            // the seed of a cluster of its own: this node
            join("$hostname:$port", port)
            return sequenceOf(request.replyWith(true))
        }
    }

    inner class ParallelJoin : PrimitiveWithSignature {
        override val signature = Signature("join", 2)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            join(request.arguments[1].toString(), request.arguments[0].toString())
            return sequenceOf(request.replyWith(true))
        }
    }

    inner class ParallelReset : PrimitiveWithSignature {
        override val signature = Signature("reset", 0)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            joined(request).second.tell(Reset)
            return sequenceOf(request.replyWith(true))
        }
    }

    inner class ParallelLeave : PrimitiveWithSignature {
        override val signature = Signature("leave", 0)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            leave()
            return sequenceOf(request.replyWith(true))
        }
    }

    // A cluster joined again replaces the previous one, which is left first
    private fun join(
        seedAddress: String,
        localPort: String,
    ) {
        leave()
        node = ClusterInitializer.joinCluster(seedAddress, localPort, hostname)
    }

    // The cluster left, if one was joined: its knowledge base is left as it is, for the nodes staying
    private fun leave() {
        val (actorSystem, _) = node ?: return
        node = null
        ClusterInitializer.leaveCluster(actorSystem)
    }

    override val alias = "prolog.argumentation.actor.solver"

    override val baseContent: Library
        get() =
            listOf(
                ParallelSolve(),
                ParallelLoad(),
                ParallelReset(),
                ParallelInit(),
                ParallelJoin(),
                ParallelLeave(),
            ).let { primitives ->
                Library.of(
                    alias = this.alias,
                    primitives = primitives.associateBy { it.signature },
                    operators = ParserModule.operators(),
                )
            }

    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override fun identifier(): String = "parallel"

    override var theoryOperators =
        ParserModule
            .operators()
            .plus(OperatorSet.DEFAULT)
}
