package it.unibo.tuprolog.argumentation.core.benchmark

import it.unibo.tuprolog.argumentation.core.TestingUtils.prepareContext
import it.unibo.tuprolog.argumentation.core.TestingUtils.solver
import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.model.Argument
import it.unibo.tuprolog.argumentation.core.model.Attack
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.core.model.LabelledArgument
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import java.lang.ref.Reference
import kotlin.random.Random
import kotlin.system.exitProcess
import kotlin.time.measureTime

/**
 * Worker JVM of [LabellingBenchmark]: runs the series given as `family:semantics:size.size...`, printing
 * `START family,n,semantics` before each case and `RESULT family,n,semantics,status,solutions,ms,heapKB` after it
 * (heapKB: the largest live heap used by the solver during the labelling, measured after a garbage collection at the
 * solutions 1, 2, 4, 8... and at the end),
 * and `SERIES DONE` at the end of each series (stopped at its first failure).
 */
object LabellingBenchmarkWorker {
    val SEMANTICS =
        listOf(
            "grounded",
            "complete",
            "preferred",
            "stable",
            "semistable",
            "ideal",
            "eager",
            "conflictfree",
            "admissible",
            "stronglyadmissible",
            "naive",
            "stage",
            "cf2",
            "stage2",
        )

    val FAMILIES: Map<String, (Int) -> Set<Pair<Int, Int>>> =
        mapOf(
            // independent mutual attacks: every argument undecided, exponentially many labellings
            "pairs" to { n: Int -> (0 until n step 2).flatMap { listOf(it to it + 1, it + 1 to it) }.toSet() },
            // mutual attacks, each pair attacking the next one: a chain of small SCCs
            "pairChain" to { n: Int ->
                (0 until n step 2)
                    .flatMap { listOf(it to it + 1, it + 1 to it) + if (it + 2 < n) listOf(it + 1 to it + 2) else emptyList() }
                    .toSet()
            },
            "evenCycle" to { n: Int -> (0 until n).map { it to (it + 1) % n }.toSet() },
            "clique" to { n: Int -> (0 until n).flatMap { a -> (0 until n).filter { it != a }.map { a to it } }.toSet() },
            "random15" to { n: Int -> random(n, 0.15) },
            "random30" to { n: Int -> random(n, 0.30) },
        )

    /**
     * Theories of growing size, built into argumentation graphs by the structured pipeline: as a family, they label
     * the graph with the semantics of the case, and report its arguments as solutions
     */
    val THEORIES: Map<String, (Int) -> String> =
        mapOf(
            // n facts propagated through 8 levels of rules, with conflicting rules and joins (as WarmJvmTest)
            "chain" to { n: Int ->
                (1..n).joinToString("\n") { "f$it :=> p0(c$it)." } + "\n" +
                    (0 until 8).joinToString("\n") { i ->
                        listOfNotNull(
                            "r$i : p$i(X) => p${i + 1}(X).",
                            if (i % 2 == 0) "n$i : p$i(X) => -p${i + 1}(X)." else null,
                            if (i >= 2) "j$i : p$i(X), p${i - 2}(X) => q$i(X)." else null,
                        ).joinToString("\n")
                    }
            },
            // n independent conflicts: two rules with opposite conclusions from the same fact, and a rule on top
            "conflicts" to { n: Int ->
                (0 until n).joinToString("\n") { "f$it :=> a$it.\nr$it : a$it => b$it.\ns$it : a$it => -b$it.\nt$it : b$it => c$it." }
            },
            // n strict rules with two premises, transposed (autoTransposition), and a defeasible rebuttal of each
            "strict" to { n: Int ->
                (0 until n).joinToString("\n") { "f$it :=> a$it.\ng$it :=> c$it.\ns$it : a$it, c$it -> b$it.\nd$it : a$it => -b$it." } +
                    "\nautoTransposition."
            },
            // a rule with n premises, each derivable in two ways (2^n arguments for its conclusion), and a rebuttal
            "fanin" to { n: Int ->
                (0 until n).joinToString("\n") { "f$it :=> p$it.\ng$it :=> q$it.\nh$it : q$it => p$it." } +
                    "\nr : " + (0 until n).joinToString(", ") { "p$it" } + " => goal.\nn : p0 => -goal."
            },
        )

    // the phases of the structured pipeline (as WarmJvmTest)
    private val PHASES =
        listOf(
            "convert" to "context_reset, parser:::convertAllRules(_)",
            "arguments" to "standard_af:::buildArguments",
            "attacks" to "standard_af:::buildAttacks",
            "extensions" to "abstract:::modifyGraph",
            "labelling" to "abstract:::buildArgumentLabelling",
            "statements" to "abstract:::buildStatementLabelling",
        )

    private fun configuration(semantics: String) =
        """
        graphBuildMode(standard_af).
        statementLabellingMode(statement).
        argumentLabellingMode($semantics).
        orderingPrinciple(last).
        orderingComparator(elitist).
        graphExtension(standardPref).
        """.trimIndent()

    /** Runs the pipeline phases on the theory, printing their times and the size of the graph as a DETAIL line */
    private fun runTheory(
        family: String,
        n: Int,
        semantics: String,
    ): Measure {
        var status = "ok"
        var arguments = 0
        var attacks = 0
        val times = mutableListOf<Long>()
        var peak = 0L
        val before = usedHeapKB()
        gcMillis = 0
        val time =
            measureTime {
                try {
                    val solver = solverWithTheory(THEORIES.getValue(family)(n) + "\n" + configuration(semantics))
                    for ((name, goal) in PHASES) {
                        val start = System.nanoTime()
                        if (solver.solve(Struct.parse(goal, solver.operators)).firstOrNull { it.isYes } == null) status = "error"
                        times += (System.nanoTime() - start) / 1_000_000
                        if (name == "extensions") {
                            val graph =
                                solver
                                    .solve(
                                        Struct.parse(
                                            "findall(A, context_check(argument(A)), As), findall(T, context_check(attack(T, _, _, _)), Ts)",
                                            solver.operators,
                                        ),
                                    ).first()
                                    .substitution
                            arguments =
                                graph.entries
                                    .first { it.key.name == "As" }
                                    .value
                                    .castToList()
                                    .toList()
                                    .size
                            attacks =
                                graph.entries
                                    .first { it.key.name == "Ts" }
                                    .value
                                    .castToList()
                                    .toList()
                                    .size
                        }
                        peak = maxOf(peak, usedHeapKB() - before)
                        if (status != "ok") break
                    }
                    Reference.reachabilityFence(solver)
                } catch (e: OutOfMemoryError) {
                    status = "oom"
                }
            }
        println("DETAIL $family,$n,$semantics,$arguments,$attacks,${times.joinToString(",")}")
        return Measure(status, arguments, time.inWholeMilliseconds - gcMillis, peak).also { gcMillis = 0 }
    }

    private fun random(
        n: Int,
        p: Double,
    ): Set<Pair<Int, Int>> {
        val r = Random(n)
        return (0 until n).flatMap { a -> (0 until n).map { b -> a to b } }.filter { (a, b) -> a != b && r.nextDouble() < p }.toSet()
    }

    /** Status, number of solutions and milliseconds of the labelling of the framework with the given semantics */
    private fun run(
        n: Int,
        attacks: Set<Pair<Int, Int>>,
        semantics: String,
    ): Measure {
        val args = (0 until n).map { Argument(listOf("ra$it"), "ra$it", "a$it") }
        var status = "ok"
        var solutions = 0
        var heapKB = -1L
        val before = usedHeapKB()
        gcMillis = 0
        val time =
            measureTime {
                try {
                    arg2pScope {
                        val solver = solver()
                        prepareContext(
                            solver,
                            Graph.of(
                                args.map { LabelledArgument(it, "und") },
                                attacks.map { (a, b) ->
                                    Attack(args[a], args[b])
                                },
                                emptyList(),
                            ),
                        )
                        var peak = 0L
                        solver.solve(semantics call "argumentLabelling").forEach {
                            when {
                                it.isHalt -> status = "error"
                                it.isYes -> {
                                    solutions++
                                    if (solutions and (solutions - 1) == 0) peak = maxOf(peak, usedHeapKB() - before)
                                }
                            }
                        }
                        heapKB = maxOf(peak, usedHeapKB() - before)
                        // otherwise the solver could be collected before the measures above, as no longer used
                        Reference.reachabilityFence(solver)
                    }
                } catch (e: OutOfMemoryError) {
                    status = "oom"
                }
            }
        // the heap measure runs garbage collections: leave it out of the time
        return Measure(status, solutions, time.inWholeMilliseconds - gcMillis, heapKB).also { gcMillis = 0 }
    }

    private data class Measure(
        val status: String,
        val solutions: Int,
        val ms: Long,
        val heapKB: Long,
    )

    private var gcMillis = 0L

    private fun usedHeapKB(): Long {
        val start = System.currentTimeMillis()
        repeat(2) { System.gc() }
        gcMillis += System.currentTimeMillis() - start
        return Runtime.getRuntime().let { (it.totalMemory() - it.freeMemory()) / 1024 }
    }

    private val repetitions = System.getProperty("benchmark.repeat")?.toIntOrNull() ?: 1

    /** The case run [repetitions] times: the median time, the heap of the first run, stopping at the first failure */
    private fun repeated(
        n: Int,
        attacks: Set<Pair<Int, Int>>,
        semantics: String,
    ): Measure {
        val runs = mutableListOf<Measure>()
        repeat(repetitions) {
            if (runs.lastOrNull()?.status?.let { it != "ok" } == true) return@repeat
            runs += run(n, attacks, semantics)
        }
        return runs.first().copy(status = runs.last().status, ms = runs.map { it.ms }.sorted()[runs.size / 2])
    }

    @JvmStatic
    fun main(args: Array<String>) {
        val series = args.map { it.split(":") }
        // warm up the JIT on a small framework
        series.map { it[1] }.distinct().forEach { run(4, FAMILIES.getValue("pairs")(4), it) }
        for ((family, semantics, sizes) in series) {
            for (n in sizes.split(".").map { it.toInt() }) {
                println("START $family,$n,$semantics")
                val result =
                    if (family in
                        THEORIES
                    ) {
                        runTheory(family, n, semantics)
                    } else {
                        repeated(n, FAMILIES.getValue(family)(n), semantics)
                    }
                println("RESULT $family,$n,$semantics,${result.status},${result.solutions},${result.ms},${result.heapKB}")
                if (result.status != "ok") break
            }
            println("SERIES DONE")
        }
        exitProcess(0)
    }
}
