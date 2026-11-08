package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.runtime.graph.GraphSharing
import it.unibo.tuprolog.argumentation.core.runtime.theory.RuleAnnotations
import it.unibo.tuprolog.argumentation.engine.vm.SolverSession
import it.unibo.tuprolog.argumentation.parallel.consumer.TheoryPartitioner
import it.unibo.tuprolog.argumentation.parallel.store.SharedGraph
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.MutableSolver
import kotlin.concurrent.thread
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/**
 * The graph built by solvers of a session together, each with its part of the rules and the pipeline as it is (see
 * SharedGraph), labelled by the first of them: the labels of the standard solver with the whole theory.
 */
class SharedGraphBuildTest {
    private var flags = Flags.defaults().queryMode(false)
    private val labelling = "context_reset, parser:::convertAllRules, structuredWhole::computeStatementLabels(In, Out, Und)"

    private fun solverOf(theory: String): MutableSolver =
        Arg2pSolverFactory.solverOf(Arg2pSolver.default(listOf(flags.library())), "").also {
            it.loadStaticKb(RuleAnnotations.parse(theory, it.operators, it.unificator))
        }

    private fun labels(
        solver: MutableSolver,
        goal: String,
    ): List<Set<String>> {
        val solution = solver.solveOnce(Struct.parse(goal, solver.operators))
        check(solution.isYes) { "$goal: $solution" }
        return listOf("In", "Out", "Und").map { name ->
            solution.substitution
                .getByName(name)!!
                .castToList()
                .toList()
                .map { it.toString() }
                .toSet()
        }
    }

    // (through text: each solver a session of its own, its segments written and read again, as across processes)
    private fun assertSameGraph(
        theory: String,
        parts: Int = 3,
    ) {
        assertSameGraph(theory, parts, throughText = false)
        assertSameGraph(theory, parts, throughText = true)
    }

    private fun assertSameGraph(
        theory: String,
        parts: Int,
        throughText: Boolean,
    ) {
        val whole = labels(Arg2pSolverFactory.default(theory, settings = flags.library()), labelling)
        val partitioning = TheoryPartitioner.partition(theory, parts, flags)
        assertTrue(partitioning.partitions.size > 1, "not split: $theory")
        val graph = SharedGraph(partitioning.partitions.size)
        val session = SolverSession().also { it[GraphSharing.KEY] = graph }
        // the solver labelling the graph, a consumer with the rules of the first partition, as on a node
        val labeller = solverOf(partitioning.theoryOf(0))
        val workers = (1 until partitioning.partitions.size).map { solverOf(partitioning.theoryOf(it)) }
        (listOf(labeller) + workers).forEach { solver ->
            if (!throughText) {
                SolverSession.join(solver, session)
            } else {
                SolverSession.join(solver, SolverSession().also { it[GraphSharing.KEY] = graph })
                if (solver !== labeller) graph.throughText(solver, null)
            }
        }
        val failures = mutableListOf<Throwable>()
        val threads =
            workers.map { worker ->
                thread {
                    try {
                        labels(
                            worker,
                            "context_reset, parser:::convertAllRules, structuredWhole::buildGraph, pipeline::modifyGraph, In = [], Out = [], Und = []",
                        )
                    } catch (failure: Throwable) {
                        synchronized(failures) { failures += failure }
                    }
                }
            }
        val shared = labels(labeller, labelling)
        threads.forEach { it.join(10_000) }
        failures.firstOrNull()?.let { throw it }
        assertEquals(whole, shared, if (throughText) "through text: $theory" else theory)
        // asked again, the labelling solver reads the graph again from the segments (its context reset), as for each goal
        assertEquals(whole, labels(labeller, labelling), "asked again: $theory")
    }

    @Test
    fun aConflictAcrossWorkers() = assertSameGraph("f1 :=> a.\nr1 : a => b.\nr2 : b => c.\nf2 :=> -b.")

    @Test
    fun undercutsOfTheRulesOfSubArguments() = assertSameGraph("r1 :=> b.\nr2 : b => c.\nr3 : c => d.\nr5 :=> undercut(r2).")

    @Test
    fun reinstatement() = assertSameGraph("r1 :=> a.\nr2 : a => b.\nr3 :=> c.\nr4 : c => -b.\nr5 :=> undercut(r4).")

    @Test
    fun strictRulesAreNotUndercut() = assertSameGraph("r1 :-> a.\nr2 : a -> b.\nr3 :=> undercut(r2).\nr4 :=> -b.")

    @Test
    fun preferences() = assertSameGraph("r1 : a => b.\nr2 : c => -b.\nf1 :=> a.\nf2 :=> c.\nsup(r1, r2).")

    // the copies of arguments of different solvers for a conflict argued attack one another
    @Test
    fun conflictsArguedAcrossSolvers() {
        flags = flags.metaConflicts(true)
        assertSameGraph("r0 : [] => conflict(a, b).\nr1 : [] => conflict(b, a).\nf1 :=> a.\nf2 :=> b.")
    }

    // a rule with no key on the conclusions of a keyed rule (the one standing on another solver than the rule's): built
    // on those of every solver
    @Test
    fun aRuleOnTheConclusionsOfAKeyedRule() =
        assertSameGraph(
            (0..7).joinToString("\n") { "fp$it :=> p(k$it)." } + "\nr(X) : p(X) => s(X).\nt : s(X) => u.\n" +
                (0..7).filter { it != 4 }.joinToString("\n") { "g$it :-> -s(k$it)." },
            parts = 4,
        )

    @Test
    fun aLongerChain() =
        assertSameGraph(
            (1..12).joinToString("\n") { "r$it : a${it - 1} => a$it." } + "\nf0 :=> a0.\nc1 :=> -a4.\nc2 :=> -a9.\nu1 :=> undercut(c2).",
            parts = 4,
        )
}
