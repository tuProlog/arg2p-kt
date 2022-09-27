package it.unibo.tuprolog.argumentation.actor.libs

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import it.unibo.tuprolog.argumentation.actor.ClusterInitializer
import it.unibo.tuprolog.argumentation.actor.actors.ResponseConsumer
import it.unibo.tuprolog.argumentation.actor.message.Add
import it.unibo.tuprolog.argumentation.actor.message.KbMessage
import it.unibo.tuprolog.argumentation.actor.message.Reset
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.libs.language.RuleParserBase
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Solve

class ActorSolver : ArgLibrary, Loadable {

    private lateinit var actorSystem: ActorSystem<KbMessage>
    private lateinit var masterActor: ActorRef<KbMessage>

    inner class ParallelSolve : PrimitiveWithSignature {

        override val signature = Signature("solve", 4)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val goal: Term = request.arguments[0]

            val response = ResponseConsumer.getResponse(goal.toString(), actorSystem, masterActor)

            return arg2pScope {
                sequenceOf(
                    request.replyWith(
                        Substitution.Companion.of(
                            mapOf(
                                Pair(request.arguments[1] as Var, response.inArgs.map { it.claim }.toTerm()),
                                Pair(request.arguments[2] as Var, response.outArgs.map { it.claim }.toTerm()),
                                Pair(request.arguments[3] as Var, response.undArgs.map { it.claim }.toTerm())
                            )
                        )
                    )
                )
            }
        }
    }

    inner class ParallelLoad : PrimitiveWithSignature {

        override val signature = Signature("load", 0)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            request.context.staticKb.forEach {
                masterActor.tell(Add(it.head.toString()))
            }
            return sequenceOf(request.replyWith(true))
        }
    }

    inner class ParallelInit : PrimitiveWithSignature {

        override val signature = Signature("join", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val port = request.arguments[0].toString()
            ClusterInitializer.joinCluster("127.0.0.1:$port", port).let {
                this@ActorSolver.actorSystem = it.first
                this@ActorSolver.masterActor = it.second
            }
            return sequenceOf(request.replyWith(true))
        }
    }

    inner class ParallelJoin : PrimitiveWithSignature {

        override val signature = Signature("join", 2)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            ClusterInitializer.joinCluster(
                request.arguments[1].toString(),
                request.arguments[0].toString()
            ).let {
                this@ActorSolver.actorSystem = it.first
                this@ActorSolver.masterActor = it.second
            }
            return sequenceOf(request.replyWith(true))
        }
    }

    inner class ParallelReset : PrimitiveWithSignature {

        override val signature = Signature("reset", 0)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            masterActor.tell(Reset)
            return sequenceOf(request.replyWith(true))
        }
    }

    inner class ParallelLeave : PrimitiveWithSignature {

        override val signature = Signature("leave", 0)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            masterActor.tell(Reset)
            ClusterInitializer.leaveCluster(actorSystem)
            return sequenceOf(request.replyWith(true))
        }
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
                ParallelLeave()
            ).let { primitives ->
                Library.of(
                    alias = this.alias,
                    primitives = primitives.associateBy { it.signature },
                    operators = RuleParserBase.operators()
                )
            }

    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override fun identifier(): String = "parallel"

    override var theoryOperators = RuleParserBase.operators()
        .plus(OperatorSet.DEFAULT)
}
