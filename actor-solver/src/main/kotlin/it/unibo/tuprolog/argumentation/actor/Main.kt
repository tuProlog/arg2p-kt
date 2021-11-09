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

enum class Label {
    IN, OUT, UND
}

interface KbMessage

data class Add(val elem: String) : KbMessage
data class RequireEvaluation(val elem: String) : KbMessage

data class Eval(val id: String, val elem: String) : KbMessage
data class EvalResponse(val id: String, val elem: String, val response: Label, val queryChain: List<String>) : KbMessage

data class FindAttacker(val id: String, val argument: String, val queryChain: List<String>, val replyTo: ActorRef<KbMessage>) : KbMessage
data class ExpectedResponses(val id: String, val number: Int) : KbMessage
data class AttackerResponse(val id: String, val argument: String, val queryChain: List<String>, val response: Label) : KbMessage

class KbDistributor private constructor(context: ActorContext<KbMessage>) : AbstractBehavior<KbMessage>(context) {

    data class HelpWorker(val id: String, val ref: ActorRef<KbMessage>, val theory: MutableSolver, val rules: MutableList<String>)
    data class EvaluationCache(val id: String, val query: String, val responseNumber: Int, val responses: MutableList<EvalResponse> = mutableListOf())

    private val workers: MutableList<HelpWorker> = mutableListOf()
    private val evaluationCache: MutableList<EvaluationCache> = mutableListOf()
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

    private fun evaluateResponses(cache: EvaluationCache) {
        if (cache.responses.size < cache.responseNumber) return

        val response = {
            if (cache.responses.any { it.response == Label.OUT }) Label.OUT
            else if (cache.responses.any { it.response == Label.IN }) Label.IN
            else Label.UND
        }

        evaluationCache.remove(cache)
        context.log.info("The result for ${cache.query} is ${response()}")
    }

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

    companion object {
        fun create(): Behavior<KbMessage> =
            Behaviors.setup { context -> KbDistributor(context) }
    }
}

class Evaluator private constructor(context: ActorContext<KbMessage>, private val master: ActorRef<KbMessage>) : AbstractBehavior<KbMessage>(context) {

    data class ActiveQuery(
        val type: Int,
        val id: String,
        var expectedResponses: Int,
        val query: String,
        val argument: String,
        val replyTo: ActorRef<KbMessage>,
        val parentId: String = "",
        var completed: Boolean = false,
        val responses: MutableList<AttackerResponse> = mutableListOf()
    )

    private val activeQueries: MutableList<ActiveQuery> = mutableListOf()

    private val solver = ClassicSolverFactory.mutableSolverWithDefaultBuiltins(
        otherLibraries = arg2p().to2pLibraries().plus(FlagsBuilder().create().baseContent),
        flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL)
    )

    override fun createReceive(): Receive<KbMessage> =
        newReceiveBuilder()
            .onMessage(Add::class.java) { command ->
                solver.assertA(Struct.parse(command.elem, arg2p().operators()))
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
                    solver.solve("structured" call "findAttacker"(Struct.parse(command.argument), listOf(command.queryChain.map(Struct::parse)), X, Y)).filter {
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

    companion object {
        fun create(master: ActorRef<KbMessage>): Behavior<KbMessage> =
            Behaviors.setup { context -> Evaluator(context, master) }
    }
}
