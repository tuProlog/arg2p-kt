package it.unibo.tuprolog.argumentation.actor.actors

import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
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
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.libs.basic.FlagsBuilder
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.classic.ClassicSolverFactory
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.Unknown
import kotlin.random.Random

class Evaluator private constructor(context: ActorContext<KbMessage>, private val master: ActorRef<KbMessage>) : AbstractBehavior<KbMessage>(context) {

    private val activeQueries: MutableList<ActiveQuery> = mutableListOf()

    override fun createReceive(): Receive<KbMessage> =
        newReceiveBuilder()
            .onMessage(Add::class.java) { command ->
                solver.assertA(Struct.parse(command.elem, solver.operators))
                arg2pScope { solver.solve("context_reset" and ("parser" call "convertAllRules"(`_`))).first() }
                this
            }
            .onMessage(Eval::class.java) { command ->
                val result = arg2pScope {
                    val query = solver.solve("parser" call "check_modifiers_in_list"("effects", listOf(Struct.parse(command.elem)), listOf(Y)))
                        .filter { it.isYes }
                        .map { it.substitution[Y]!! }
                        .first()
                    solver.solve("structured" call "buildArgument"(query, X)).filter {
                        it.isYes
                    }.map {
                        it.substitution[X]!!
                    }
                }.toList()
                if (result.isEmpty()) {
                    master.tell(EvalResponse(command.id, command.elem, Label.UND, listOf()))
                }
                result.forEach {
                    val query = ActiveQuery(0, "query_${Random.nextInt(0, Int.MAX_VALUE)}", -1, command.elem, it.toString(), master, parentId = command.id)
                    activeQueries.add(query)
                    master.tell(FindAttacker(query.id, it.toString(), listOf(it.toString()), context.self))
                }
                this
            }
            .onMessage(ExpectedResponses::class.java) { command ->
                activeQueries.first { it.id == command.id }.expectedResponses = command.number
                this
            }
            .onMessage(FindAttacker::class.java) { command ->
                val result = arg2pScope {
                    solver.solve(
                        "structured" call "findAttacker"(
                            Struct.parse(command.argument),
                            listOf(
                                command.queryChain.map(
                                    Struct::parse
                                )
                            ),
                            X,
                            Y
                        )
                    ).filter {
                        it.isYes
                    }.map {
                        Pair(it.substitution[X]!!.toString(), it.substitution[Y]!!.toString())
                    }
                }.toList()
                if (result.isEmpty()) {
                    command.replyTo.tell(AttackerResponse(command.id, "", command.queryChain, Label.OUT))
                }
                result.forEach {
                    if (it.second == "no") {
                        command.replyTo.tell(AttackerResponse(command.id, it.first, command.queryChain + it.first, Label.UND))
                    } else {
                        val query = ActiveQuery(1, "query_${Random.nextInt(0, Int.MAX_VALUE)}", -1, command.argument, it.first, command.replyTo, parentId = command.id)
                        activeQueries.add(query)
                        master.tell(FindAttacker(query.id, query.argument, command.queryChain + it.first, this@Evaluator.context.self))
                    }
                }
                this
            }
            .onMessage(AttackerResponse::class.java) { command ->
                val active = activeQueries.first { it.id == command.id }
                if (active.completed) return@onMessage this
                val reply = { label: Label ->
                    if (active.type == 0) {
                        active.replyTo.tell(EvalResponse(active.parentId, active.query, label, command.queryChain))
                    } else {
                        active.replyTo.tell(AttackerResponse(active.parentId, active.argument, command.queryChain, label))
                    }
                }
                when (command.response) {
                    Label.IN -> {
                        reply(Label.OUT)
                        active.completed = true
                        return@onMessage this
                    }
                    else -> {
                        active.responses.add(command)
                    }
                }
                active.completed = active.expectedResponses == active.responses.count()
                if (active.completed && active.responses.any { it.response == Label.UND }) {
                    reply(Label.UND)
                } else if (active.completed) {
                    reply(Label.IN)
                }
                this
            }
            .build()

    private val solver = ClassicSolverFactory.mutableSolverWithDefaultBuiltins(
        otherLibraries = Arg2pSolver.default(listOf(FlagsBuilder().create()))
            .to2pLibraries(),
        flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL)
    )

    companion object {
        fun create(master: ActorRef<KbMessage>): Behavior<KbMessage> =
            Behaviors.setup { context -> Evaluator(context, master) }
    }
}
