package it.unibo.tuprolog.argumentation.actor.actors.splitting

import akka.actor.typed.javadsl.ActorContext
import akka.cluster.sharding.typed.javadsl.ClusterSharding
import akka.cluster.sharding.typed.javadsl.EntityTypeKey
import it.unibo.tuprolog.argumentation.actor.actors.HelpWorker
import it.unibo.tuprolog.argumentation.actor.libs.TheoryChainer
import it.unibo.tuprolog.argumentation.actor.message.Add
import it.unibo.tuprolog.argumentation.actor.message.KbMessage
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.engine.Arg2pPrologSolverFactory
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import kotlin.random.Random

internal object SupportSplit : SplittingPrinciple {
    override fun updateWorkers(
        rule: String,
        kb: SplitKnowledgeBase,
        context: ActorContext<KbMessage>,
    ) {
        val helperSolver = solver()
        helperSolver.loadStaticKb(Theory.parse("$rule.", helperSolver.operators))
        arg2pScope {
            helperSolver
                .solve("parser" call "convertAllRules"(X))
                .filter { it.isYes }
                .map { it.substitution[X]!! }
                .first()
                .also { rules ->
                    val selected =
                        kb.workers.filter { elem ->
                            elem.theory
                                .solve("chainer" call "chainable"(rules))
                                .filter { it.isYes }
                                .any()
                        }
                    when (selected.count()) {
                        0 -> createSolver(rule, kb, context)
                        1 -> updateSolverKnowledge(rule, selected.first(), context)
                        else -> mergeSolvers(rule, selected, kb, context)
                    }
                }
        }
    }

    private fun createWorker(
        rules: MutableList<String>,
        context: ActorContext<KbMessage>,
    ): HelpWorker =
        solver().let { solver ->
            val id = "solver_${Random.nextInt()}"
            val ref =
                ClusterSharding
                    .get(context.system)
                    .entityRefFor(EntityTypeKey.create(KbMessage::class.java, "evaluator"), id)
            val worker = HelpWorker(id, ref, solver, rules)
            rules.forEach {
                worker.ref.tell(Add(it))
                worker.theory.assertA(Struct.parse(it, worker.theory.operators))
            }
            arg2pScope { solver.solve("context_reset" and ("parser" call "convertAllRules"(`_`))).first() }
            worker
        }

    private fun createSolver(
        rule: String,
        kb: SplitKnowledgeBase,
        context: ActorContext<KbMessage>,
    ) {
        context.log.info("Creating $rule")
        kb.add(createWorker(mutableListOf(rule), context))
    }

    private fun updateSolverKnowledge(
        rule: String,
        worker: HelpWorker,
        context: ActorContext<KbMessage>,
    ) {
        context.log.info("Updating $rule")
        worker.theory.assertA(Struct.parse(rule, worker.theory.operators))
        worker.rules.add(rule)
        arg2pScope { worker.theory.solve("context_reset" and ("parser" call "convertAllRules"(`_`))).first() }
        worker.ref.tell(Add(rule))
    }

    // The workers whose rules chain with [rule] replaced by one with all of them
    private fun mergeSolvers(
        rule: String,
        oldWorkers: List<HelpWorker>,
        kb: SplitKnowledgeBase,
        context: ActorContext<KbMessage>,
    ) {
        context.log.info("Merging $rule")
        kb.remove(oldWorkers)
        kb.add(createWorker(oldWorkers.flatMap { it.rules }.toMutableList().also { it.add(rule) }, context))
    }

    private fun solver() =
        Arg2pPrologSolverFactory.mutableSolverWithDefaultBuiltins(
            otherLibraries =
                Arg2pSolver
                    .default(listOf(FlagsBuilder().create()), listOf(TheoryChainer))
                    .to2pLibraries(),
            flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL),
        )
}
