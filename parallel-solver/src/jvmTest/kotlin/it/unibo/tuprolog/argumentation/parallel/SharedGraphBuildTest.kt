package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.engine.vm.SolverSession
import it.unibo.tuprolog.argumentation.core.primitives.graph.SharedGraph
import it.unibo.tuprolog.argumentation.core.runtime.theory.RuleAnnotations
import it.unibo.tuprolog.argumentation.parallel.partition.TheoryPartitioner
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.MutableSolver
import kotlin.concurrent.thread
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/**
 * The graph built by solvers of a session together, each with its part of the rules and the pipeline as it is (see
 * SharedGraph), labelled by one with no rule of its own: the labels of the standard solver with the whole theory.
 */
class SharedGraphBuildTest {
    private val flags = Flags.defaults().queryMode(false)
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

    private fun assertSameGraph(
        theory: String,
        parts: Int = 3,
    ) {
        val whole = labels(Arg2pSolverFactory.default(theory, settings = flags.library()), labelling)
        val partitioning = TheoryPartitioner.partition(theory, parts, flags)
        assertTrue(partitioning.partitions.size > 1, "not split: $theory")
        val session = SolverSession()
        session.shared = SharedGraph(partitioning.partitions.size + 1)
        val workers = partitioning.partitions.indices.map { solverOf(partitioning.theoryOf(it)) }
        val catalog = solverOf(partitioning.catalog)
        (workers + catalog).forEach { SolverSession.join(it, session) }
        val failures = mutableListOf<Throwable>()
        val threads =
            workers.map { worker ->
                thread {
                    try {
                        labels(worker, "context_reset, parser:::convertAllRules, structuredWhole::buildGraph, In = [], Out = [], Und = []")
                    } catch (failure: Throwable) {
                        synchronized(failures) { failures += failure }
                    }
                }
            }
        val shared = labels(catalog, labelling)
        threads.forEach { it.join(10_000) }
        failures.firstOrNull()?.let { throw it }
        assertEquals(whole, shared, theory)
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

    @Test
    fun aLongerChain() =
        assertSameGraph(
            (1..12).joinToString("\n") { "r$it : a${it - 1} => a$it." } + "\nf0 :=> a0.\nc1 :=> -a4.\nc2 :=> -a9.\nu1 :=> undercut(c2).",
            parts = 4,
        )
}
