package it.unibo.tuprolog.argumentation.actor.actors

import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.SupervisorStrategy
import akka.actor.typed.javadsl.AbstractBehavior
import akka.actor.typed.javadsl.ActorContext
import akka.actor.typed.javadsl.Behaviors
import akka.actor.typed.javadsl.Receive
import it.unibo.tuprolog.argumentation.actor.message.Add
import it.unibo.tuprolog.argumentation.actor.message.AttackerResponse
import it.unibo.tuprolog.argumentation.actor.message.Eval
import it.unibo.tuprolog.argumentation.actor.message.EvalResponse
import it.unibo.tuprolog.argumentation.actor.message.ExpectedResponses
import it.unibo.tuprolog.argumentation.actor.message.FindAttacker
import it.unibo.tuprolog.argumentation.actor.message.KbMessage
import it.unibo.tuprolog.argumentation.actor.message.Label
import it.unibo.tuprolog.argumentation.actor.message.Stop
import it.unibo.tuprolog.argumentation.actor.message.WorkerResponses
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.engine.Arg2pPrologSolverFactory
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.Unknown
import kotlin.random.Random

internal class Evaluator private constructor(
    context: ActorContext<KbMessage>,
    private val master: ActorRef<KbMessage>,
    private val entityId: String,
) : AbstractBehavior<KbMessage>(
        context,
    ) {
    private val activeQueries: MutableList<ActiveQuery> = mutableListOf()

    override fun createReceive(): Receive<KbMessage> =
        newReceiveBuilder()
            .onMessage(Add::class.java) { command ->
                solver.assertA(Struct.parse(command.elem, solver.operators))
                arg2pScope { solver.solve("context_reset" and ("parser" call "convertAllRules"(`_`))).first() }
                this
            }.onMessage(Eval::class.java) { command ->
                val result =
                    arg2pScope {
                        solver
                            .solve("structuredPartial" call "buildArgument"(Struct.parse(command.elem), X, Y))
                            .filter {
                                it.isYes
                            }.map {
                                Pair(it.substitution[X]!!.toString(), it.substitution[Y]!!.strings())
                            }
                    }.toList()
                master.tell(WorkerResponses(command.id, maxOf(result.size, 1)))
                if (result.isEmpty()) {
                    master.tell(EvalResponse(command.id, command.elem, Label.NOT_FOUND, listOf()))
                }
                result.forEach { (argument, subArguments) ->
                    val query =
                        ActiveQuery(
                            0,
                            "query_${Random.nextInt(0, Int.MAX_VALUE)}",
                            command.elem,
                            argument,
                            master,
                            parentId = command.id,
                        )
                    activeQueries.add(query)
                    master.tell(FindAttacker(query.id, argument, subArguments, listOf(argument), context.self))
                }
                this
            }.onMessage(ExpectedResponses::class.java) { command ->
                activeQueries.firstOrNull { it.id == command.id }?.also {
                    it.expected.workers = command.number
                    conclude(it)
                }
                this
            }.onMessage(WorkerResponses::class.java) { command ->
                activeQueries.firstOrNull { it.id == command.id }?.also {
                    it.expected.report(command.number)
                    conclude(it)
                }
                this
            }.onMessage(FindAttacker::class.java) { command ->
                val result =
                    arg2pScope {
                        solver
                            .solve(
                                "structuredPartial" call
                                    "findAttacker"(
                                        Struct.parse(command.argument),
                                        logicListOf(command.subArguments.map(Struct::parse)),
                                        logicListOf(command.queryChain.map(Struct::parse)),
                                        X,
                                        Y,
                                        Z,
                                    ),
                            ).filter {
                                it.isYes
                            }.map {
                                Triple(it.substitution[X]!!.toString(), it.substitution[Y]!!.strings(), it.substitution[Z]!!.toString())
                            }
                    }.toList()
                command.replyTo.tell(WorkerResponses(command.id, maxOf(result.size, 1)))
                if (result.isEmpty()) {
                    command.replyTo.tell(AttackerResponse(command.id, "", command.queryChain, Label.OUT))
                }
                result.forEach { (attacker, subArguments, isValid) ->
                    if (isValid == "no") {
                        command.replyTo.tell(AttackerResponse(command.id, attacker, command.queryChain + attacker, Label.UND))
                    } else {
                        val query =
                            ActiveQuery(
                                1,
                                "query_${Random.nextInt(0, Int.MAX_VALUE)}",
                                command.argument,
                                attacker,
                                command.replyTo,
                                parentId = command.id,
                            )
                        activeQueries.add(query)
                        master.tell(
                            FindAttacker(query.id, attacker, subArguments, command.queryChain + attacker, this@Evaluator.context.self),
                        )
                    }
                }
                this
            }.onMessage(AttackerResponse::class.java) { command ->
                val active = activeQueries.firstOrNull { it.id == command.id } ?: return@onMessage this
                if (active.completed) return@onMessage this
                if (command.response == Label.IN) {
                    active.completed = true
                    reply(active, Label.OUT, command.queryChain)
                } else {
                    active.responses.add(command)
                    conclude(active)
                }
                this
            }.onMessage(Stop::class.java) {
                // its rules given to another worker, or dropped with the knowledge base: its solver freed
                Behaviors.stopped()
            }.build()

    // The label of the argument of a query, once every worker has sent its responses: in when no attacker is in or
    // und (an attacker in makes it out as soon as it comes)
    private fun conclude(active: ActiveQuery) {
        if (active.completed || !active.expected.reached(active.responses.size)) return
        active.completed = true
        val label = if (active.responses.any { it.response == Label.UND }) Label.UND else Label.IN
        reply(
            active,
            label,
            active.responses
                .lastOrNull()
                ?.queryChain
                .orEmpty(),
        )
    }

    private fun reply(
        active: ActiveQuery,
        label: Label,
        queryChain: List<String>,
    ) = if (active.type == 0) {
        active.replyTo.tell(EvalResponse(active.parentId, active.query, label, queryChain))
    } else {
        active.replyTo.tell(AttackerResponse(active.parentId, active.argument, queryChain, label))
    }

    private fun Term.strings() = castToList().toList().map { it.toString() }

    private val solver =
        Arg2pPrologSolverFactory.mutableSolverWithDefaultBuiltins(
            otherLibraries =
                Arg2pSolver
                    .default(listOf(FlagsBuilder(graphExtensions = emptyList()).create()))
                    .to2pLibraries(),
            flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL),
        )

    companion object {
        // resumed on a failure, keeping its rules, rather than stopped
        fun create(
            master: ActorRef<KbMessage>,
            entityId: String,
        ): Behavior<KbMessage> =
            Behaviors
                .supervise(Behaviors.setup { context -> Evaluator(context, master, entityId) })
                .onFailure(SupervisorStrategy.resume())
    }
}
