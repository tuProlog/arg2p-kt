package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.graph.GraphSharing
import it.unibo.tuprolog.argumentation.core.notation.RuleAnnotations
import it.unibo.tuprolog.argumentation.engine.vm.SolverSession
import it.unibo.tuprolog.argumentation.parallel.consumer.TheoryPartitioner
import it.unibo.tuprolog.argumentation.parallel.store.SharedGraph
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.MutableSolver
import kotlin.concurrent.thread
import kotlin.test.Test

/**
 * Steady state of the graph built by solvers of a session together (see SharedGraph) against one solver: run only with
 * SHARED_BENCH set ("family size parts runs", several separated by ';'), printing the best wall time of the runs.
 */
class SharedGraphBench {
    private val flags = Flags.defaults().queryMode(false)
    private val labelling = "context_reset, parser:::convertAllRules, structuredWhole::computeStatementLabels(In, Out, Und)"

    private fun indep(n: Int): String =
        (0 until n).joinToString("\n") { b ->
            (
                listOf("f$b :=> l${b}_0.") +
                    (1..6).flatMap { i -> listOf("a${b}_$i : l${b}_${i - 1} => l${b}_$i.", "b${b}_$i : l${b}_${i - 1} => l${b}_$i.") } +
                    listOf("p$b : l${b}_6 => g$b.", "q$b : f${b}f => -g$b.", "ff$b :=> f${b}f.")
            ).joinToString("\n")
        }

    private fun dense(n: Int): String =
        (0 until n).joinToString("\n") { b ->
            listOf("x" to "", "y" to "-").joinToString("\n") { (side, sign) ->
                (
                    listOf("f$side$b :=> $side${b}_0.") +
                        (1..6).flatMap { i ->
                            listOf(
                                "a$side${b}_$i : $side${b}_${i - 1} => $side${b}_$i.",
                                "b$side${b}_$i : $side${b}_${i - 1} => $side${b}_$i.",
                            )
                        } +
                        listOf("t$side$b : $side${b}_6 => ${sign}g$b.")
                ).joinToString("\n")
            }
        }

    private fun firstorder(n: Int): String =
        (
            (0 until n).map { "fp$it :=> p0($it)." } + (1..4).map { k -> "r$k(X) : p${k - 1}(X) => p$k(X)." } +
                listOf("last(X) : p4(X) => done(X).") + (0 until n step 2).map { "fe$it :=> e($it)." } + listOf("no(X) : e(X) => -done(X).")
        ).joinToString("\n")

    private fun solverOf(theory: String): MutableSolver =
        Arg2pSolverFactory.solverOf(Arg2pSolver.default(listOf(flags.library())), "").also {
            it.loadStaticKb(RuleAnnotations.parse(theory, it.operators, it.unificator))
        }

    private fun solved(
        solver: MutableSolver,
        goal: String,
    ): String {
        val solution = solver.solveOnce(Struct.parse(goal, solver.operators))
        check(solution.isYes) { "$goal: $solution" }
        return solution.substitution
            .getByName("In")
            .toString()
            .length
            .toString()
    }

    private fun <T> timed(block: () -> T): Pair<Double, T> {
        val start = System.nanoTime()
        val result = block()
        return (System.nanoTime() - start) / 1e9 to result
    }

    @Test
    fun bench() {
        val spec = System.getenv("SHARED_BENCH") ?: return
        for (case in spec.split(";").map { it.trim().split(" ") }) {
            val (family, size, parts, runs) = case
            val theory =
                when (family) {
                    "indep" -> indep(size.toInt())
                    "dense" -> dense(size.toInt())
                    else -> firstorder(size.toInt())
                }
            val whole =
                (1..runs.toInt()).map {
                    timed {
                        solved(
                            Arg2pSolverFactory.default(theory, settings = flags.library()),
                            labelling,
                        )
                    }
                }
            val (partitionTime, partitioning) = timed { TheoryPartitioner.partition(theory, parts.toInt(), flags) }
            val shared =
                (1..runs.toInt()).map {
                    timed {
                        val session = SolverSession()
                        session[GraphSharing.KEY] = SharedGraph(partitioning.partitions.size)
                        // the solver labelling the graph, a consumer with the rules of the first partition, as on a node
                        val labeller = solverOf(partitioning.theoryOf(0))
                        val workers = (1 until partitioning.partitions.size).map { solverOf(partitioning.theoryOf(it)) }
                        (listOf(labeller) + workers).forEach { SolverSession.join(it, session) }
                        val threads =
                            workers.map { worker ->
                                thread {
                                    worker.solveOnce(
                                        Struct.parse(
                                            "context_reset, parser:::convertAllRules, structuredWhole::buildGraph, pipeline::modifyGraph",
                                            worker.operators,
                                        ),
                                    )
                                }
                            }
                        val labels = solved(labeller, labelling)
                        threads.forEach { it.join() }
                        labels
                    }
                }
            check(whole.map { it.second }.toSet() == shared.map { it.second }.toSet()) { "labels differ: $family $size" }
            println(
                "BENCH $family $size: whole best %.3f s (%s); shared %d parts best %.3f s (%s); partition %.3f s".format(
                    whole.minOf { it.first },
                    whole.joinToString(" ") { "%.2f".format(it.first) },
                    partitioning.partitions.size,
                    shared.minOf { it.first },
                    shared.joinToString(" ") { "%.2f".format(it.first) },
                    partitionTime,
                ),
            )
        }
    }
}
