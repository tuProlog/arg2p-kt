package it.unibo.tuprolog.argumentation.actor.libs

import akka.actor.typed.ActorSystem
import it.unibo.tuprolog.argumentation.actor.actors.KbDistributor
import it.unibo.tuprolog.argumentation.actor.actors.ResponseConsumer
import it.unibo.tuprolog.argumentation.actor.message.Add
import it.unibo.tuprolog.argumentation.actor.message.Reset
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.BaseArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.PrimitiveWithSignature
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Solve

class ActorSolver : BaseArgLibrary(), Loadable {

    private val actorSystem = ActorSystem.create(KbDistributor.create(), "solver")

    inner class ParallelSolve : PrimitiveWithSignature {

        override val signature = Signature("solve", 4)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val goal: Term = request.arguments[0]

            val response = ResponseConsumer.getResponse(goal.toString(), actorSystem)

            return arg2pScope {
                sequenceOf(request.replyWith(Substitution.Companion.of(mapOf(
                    Pair(request.arguments[1] as Var, listOf(response.inArgs.map { it.claim })),
                    Pair(request.arguments[2] as Var, listOf(response.outArgs.map { it.claim })),
                    Pair(request.arguments[3] as Var, listOf(response.undArgs.map { it.claim }))
                ))))
            }
        }
    }

    inner class ParallelLoad : PrimitiveWithSignature {

        override val signature = Signature("load", 0)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            request.context.staticKb.forEach {
                actorSystem.tell(Add(it.toString()))
            }
            return sequenceOf(request.replyWith(true))
        }
    }

    inner class ParallelReset : PrimitiveWithSignature {

        override val signature = Signature("reset", 0)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            actorSystem.tell(Reset)
            return sequenceOf(request.replyWith(true))
        }
    }

    override val alias = "prolog.argumentation.actor.solver"

    override val baseContent: AliasedLibrary
        get() =
            listOf(
                ParallelSolve(),
                ParallelLoad(),
                ParallelReset()
            ).let { primitives ->
                Library.aliased(
                    alias = this.alias,
                    primitives = primitives.associateBy { it.signature }
                )
            }

    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()


    override fun identifier(): String = "parallel"
}
