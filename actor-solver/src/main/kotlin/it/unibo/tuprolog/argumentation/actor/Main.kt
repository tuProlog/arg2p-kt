package it.unibo.tuprolog.argumentation.actor

import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.javadsl.AbstractBehavior
import akka.actor.typed.javadsl.ActorContext
import akka.actor.typed.javadsl.Behaviors
import akka.actor.typed.javadsl.Receive
import it.unibo.tuprolog.argumentation.core.arg2p
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.libs.basic.FlagsBuilder
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.classic.ClassicSolverFactory
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import kotlin.random.Random

interface KbMessage
data class Add(val elem: String) : KbMessage
data class Eval(val elem: String) : KbMessage
data class Response(val elem: String, val response: String) : KbMessage

class KbDistributor private constructor(context: ActorContext<KbMessage>) : AbstractBehavior<KbMessage>(context) {

    data class HelpWorker(val id: String, val ref: ActorRef<KbMessage>, val theory: MutableSolver, val rules: MutableList<String>)

    private val workers: MutableList<HelpWorker> = mutableListOf()
    private val actorLibrary = Library.aliased(
        alias = "prolog.argumentation.actors.helpers",
        theory = Theory.parse(
            """            
            chainable(X) :-
                member(rule([_, Prem, _]), X),
                member(P, Prem),
                (context_check(rule([_, _, P])); context_check(premise([_, P]))).
                
            chainable(X) :- 
                member(rule([_, _, Conclusion]), X),
                context_check(rule([_, Prem, _])),
                member(Conclusion, Prem).
                
            chainable(X) :- 
                member(premise([_, Conclusion]), X),
                context_check(rule([_, Prem, _])),
                member(Conclusion, Prem).
            """.trimIndent()
        ),
    )
    private val helperSolver = solver()

    private fun solver() = ClassicSolverFactory.mutableSolverWithDefaultBuiltins(
        otherLibraries = arg2p().to2pLibraries()
            .plus(FlagsBuilder().create().baseContent)
            .plus(actorLibrary),
        flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL)
    )

    private fun createSolver(rule: String) =
        solver().also {
            context.log.info("Creating $rule")
            val id = "solver_${Random.nextInt()}"
            val worker = HelpWorker(id, context.spawn(Evaluator.create(context.self), id), it, mutableListOf(rule))
            workers.add(worker)
            it.assertA(Struct.parse(rule, it.operators))
            arg2pScope { it.solve("context_reset" and ("parser" call "convertAllRules"(`_`))).first() }
            worker.ref.tell(Add(rule))
        }

    private fun updateSolverKnowledge(worker: HelpWorker, rule: String) {
        context.log.info("Updating $rule")
        worker.theory.assertA(Struct.parse(rule, worker.theory.operators))
        worker.rules.add(rule)
        arg2pScope { worker.theory.solve("context_reset" and ("parser" call "convertAllRules"(`_`))).first() }
        worker.ref.tell(Add(rule))
    }

    private fun mergeSolvers(oldWorkers: List<HelpWorker>, rule: String) {
        context.log.info("Merging $rule")
        workers.removeAll(oldWorkers)
        val kb = oldWorkers.flatMap { it.rules }.toMutableList().also { it.add(rule) }
        oldWorkers.forEach { context.stop(it.ref) }
        solver().also { solver ->
            val id = "solver_${Random.nextInt()}"
            val worker = HelpWorker(id, context.spawn(Evaluator.create(context.self), id), solver, kb)
            workers.add(worker)
            kb.forEach {
                worker.ref.tell(Add(it))
                worker.theory.assertA(Struct.parse(it, worker.theory.operators))
            }
            arg2pScope { solver.solve("context_reset" and ("parser" call "convertAllRules"(`_`))).first() }
        }
    }

    private fun updateWorkers(rule: String) {
        helperSolver.loadStaticKb(Theory.parse("$rule.", helperSolver.operators))
        arg2pScope {
            helperSolver.solve("parser" call "convertAllRules"(X))
                .filter { it.isYes }
                .map { it.substitution[X]!! }
                .first().also { rules ->
                    val selected = workers.filter { elem ->
                        elem.theory.solve("chainable"(rules)).filter { it.isYes }.any()
                    }
                    when (selected.count()) {
                        0 -> createSolver(rule)
                        1 -> updateSolverKnowledge(selected.first(), rule)
                        else -> mergeSolvers(selected, rule)
                    }
                }
        }
    }

    override fun createReceive(): Receive<KbMessage> =
        newReceiveBuilder()
            .onMessage(Add::class.java) { command ->
                context.log.info("Adding ${command.elem}")
                updateWorkers(command.elem)
                this
            }
            .onMessage(Eval::class.java) { command ->
                context.log.info("Eval for ${command.elem}")
                workers.forEach { it.ref.tell(command) }
                this
            }
            .onMessage(Response::class.java) { command ->
                context.log.info("The result for ${command.elem} is ${command.response}")
                this
            }
            .build()

    companion object {
        fun create(): Behavior<KbMessage> =
            Behaviors.setup { context -> KbDistributor(context) }
    }
}

class Evaluator private constructor(context: ActorContext<KbMessage>, private val master: ActorRef<KbMessage>) : AbstractBehavior<KbMessage>(context) {

    private val solver = ClassicSolverFactory.mutableSolverWithDefaultBuiltins(
        otherLibraries = arg2p().to2pLibraries().plus(FlagsBuilder().create().baseContent),
        flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL)
    )

    override fun createReceive(): Receive<KbMessage> =
        newReceiveBuilder()
            .onMessage(Add::class.java) { command ->
                solver.assertA(Struct.parse(command.elem, arg2p().operators()))
                this
            }
            .onMessage(Eval::class.java) { command ->
                val result = arg2pScope {
                    solver.solve("answerQuery"(command.elem, X, Y, Z)).filter {
                        it.isYes
                    }.map {
                        if (!(it.substitution[X]?.isEmptyList)!!) "IN"
                        else if (!(it.substitution[Y]?.isEmptyList)!!) "OUT"
                        else "UND"
                    }.first()
                }
                master.tell(Response(command.elem, result))
                this
            }
            .build()

    companion object {
        fun create(master: ActorRef<KbMessage>): Behavior<KbMessage> =
            Behaviors.setup { context -> Evaluator(context, master) }
    }
}

// Distribution OK
// Solving ...
