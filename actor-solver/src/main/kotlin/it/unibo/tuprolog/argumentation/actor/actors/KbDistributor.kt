package it.unibo.tuprolog.argumentation.actor.actors

import akka.actor.typed.Behavior
import akka.actor.typed.javadsl.AbstractBehavior
import akka.actor.typed.javadsl.ActorContext
import akka.actor.typed.javadsl.Behaviors
import akka.actor.typed.javadsl.Receive
import it.unibo.tuprolog.argumentation.actor.libs.TheoryChainer
import it.unibo.tuprolog.argumentation.actor.message.Add
import it.unibo.tuprolog.argumentation.actor.message.Eval
import it.unibo.tuprolog.argumentation.actor.message.EvalResponse
import it.unibo.tuprolog.argumentation.actor.message.ExpectedResponses
import it.unibo.tuprolog.argumentation.actor.message.FindAttacker
import it.unibo.tuprolog.argumentation.actor.message.KbMessage
import it.unibo.tuprolog.argumentation.actor.message.Label
import it.unibo.tuprolog.argumentation.actor.message.RequireEvaluation
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.libs.basic.FlagsBuilder
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.classic.ClassicSolverFactory
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import kotlin.random.Random

class KbDistributor private constructor(context: ActorContext<KbMessage>) : AbstractBehavior<KbMessage>(context) {

    private val helperSolver = solver()
    private val workers: MutableList<HelpWorker> = mutableListOf()
    private val evaluationCache: MutableList<EvaluationCache> = mutableListOf()

    override fun createReceive(): Receive<KbMessage> =
        newReceiveBuilder()
            .onMessage(Add::class.java) { command ->
                context.log.info("Adding ${command.elem}")
                updateWorkers(command.elem)
                this
            }
            .onMessage(RequireEvaluation::class.java) { command ->
                context.log.info("Eval for ${command.elem}")
                val cache = EvaluationCache("eval_${Random.nextInt(0, Int.MAX_VALUE)}", command.elem, workers.size)
                evaluationCache.add(cache)
                workers.forEach { it.ref.tell(Eval(cache.id, command.elem)) }
                this
            }
            .onMessage(EvalResponse::class.java) { command ->
                val cache = evaluationCache.first { it.id == command.id }
                cache.responses.add(command)
                evaluateResponses(cache)
                this
            }.onMessage(FindAttacker::class.java) { command ->
                command.replyTo.tell(ExpectedResponses(command.id, workers.count()))
                workers.forEach { it.ref.tell(command) }
                this
            }
            .build()

    private fun solver() = ClassicSolverFactory.mutableSolverWithDefaultBuiltins(
        otherLibraries = Arg2pSolver.default(listOf(FlagsBuilder().create()), listOf(TheoryChainer))
            .to2pLibraries(),
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
                        elem.theory.solve("chainer" call "chainable"(rules)).filter { it.isYes }.any()
                    }
                    when (selected.count()) {
                        0 -> createSolver(rule)
                        1 -> updateSolverKnowledge(selected.first(), rule)
                        else -> mergeSolvers(selected, rule)
                    }
                }
        }
    }

    private fun evaluateResponses(cache: EvaluationCache) {
        if (cache.responses.size < cache.responseNumber) return

        evaluationCache.remove(cache)
        context.log.info(
            "The result for ${cache.query} is ${
            if (cache.responses.any { it.response == Label.OUT }) Label.OUT
            else if (cache.responses.any { it.response == Label.IN }) Label.IN
            else Label.UND
            }"
        )
    }

    companion object {
        fun create(): Behavior<KbMessage> =
            Behaviors.setup { context -> KbDistributor(context) }
    }
}
