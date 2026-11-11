package it.unibo.tuprolog.argumentation.core.benchmark

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.solve.prolog.PrologSolverFactory
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import java.io.File
import java.lang.management.ManagementFactory
import kotlin.test.Test

/**
 * The libraries of arg2p on its machine and on the solver of tuProlog (solve-prolog, as StandardEngineTest builds it): the
 * CPU time, the wall-clock time (finer: the CPU time of a thread ticks every 15 ms on Windows) and the bytes allocated by the
 * solving thread for each workload, the median of five solves on a fresh solver after one to warm up (a single one for a
 * workload over [budget] milliseconds). Run with
 * `./gradlew :core:jvmBenchmark --tests "*TuPrologEngineBenchmark"`, passing as -P project properties:
 * - benchmark.cases: a directory of theories (`name.pl`, with their flags in `name.flags`), solved whole, besides the workloads
 * - benchmark.engines: `arg2p`, `tuprolog` or both (the default)
 * - benchmark.output: the CSV report (default build/reports/benchmark/tuprolog.csv)
 */
class TuPrologEngineBenchmark {
    private fun property(name: String) = System.getProperty("benchmark.$name")?.takeIf { it.isNotBlank() }

    private val budget = 20_000L

    private class Workload(
        val theory: String,
        val flags: String,
        val goal: String,
    )

    private val config =
        "statementLabellingMode(statement).\nargumentLabellingMode(grounded).\norderingPrinciple(last).\n" +
            "orderingComparator(elitist).\ngraphExtension(standardPref).\nqueryMode(off)."

    private val workloads: Map<String, Workload> by lazy {
        val theories = LabellingBenchmarkWorker.THEORIES
        val built =
            linkedMapOf(
                "graph-chain" to Workload(theories.getValue("chain")(40), config, "solve(_, _, _)"),
                "graph-conflicts" to Workload(theories.getValue("conflicts")(100), config, "solve(_, _, _)"),
                "graph-strict" to Workload(theories.getValue("strict")(100), config, "solve(_, _, _)"),
                "query-chain" to
                    Workload(theories.getValue("chain")(40), config.replace("queryMode(off).", "queryMode."), "solve(p8(X), _, _, _)"),
            )
        property("cases")?.let { dir ->
            File(dir).listFiles { it -> it.extension == "pl" }.orEmpty().sortedBy { it.name }.forEach { theory ->
                val flags = File(theory.parentFile, theory.nameWithoutExtension + ".flags").takeIf { it.exists() }?.readText().orEmpty()
                built[theory.nameWithoutExtension] = Workload(theory.readText(), flags, "solve(_, _, _)")
            }
        }
        built
    }

    private fun arg2p(workload: Workload): Solver =
        Arg2pSolverFactory.default(theory = workload.theory, settings = Flags.parse(workload.flags).library())

    private fun tuProlog(workload: Workload): Solver {
        val arg2p = Arg2pSolver.default(listOf(Flags.parse(workload.flags).library()))
        return PrologSolverFactory.mutableSolverWithDefaultBuiltins(
            otherLibraries = arg2p.to2pLibraries(),
            flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL),
            staticKb = Theory.parse(workload.theory, arg2p.operators()),
        )
    }

    private class Measure(
        val ms: Long,
        val wall: Double,
        val bytes: Long,
    )

    private fun run(
        workload: Workload,
        solverOf: (Workload) -> Solver,
    ): Measure {
        val threads = ManagementFactory.getThreadMXBean() as com.sun.management.ThreadMXBean
        val id = Thread.currentThread().id
        val solver = solverOf(workload)
        val goal = Struct.parse(workload.goal, solver.operators)
        val bytes = threads.getThreadAllocatedBytes(id)
        val start = threads.currentThreadCpuTime
        val wall = System.nanoTime()
        val solution = solver.solve(goal).first()
        val measure =
            Measure(
                (threads.currentThreadCpuTime - start) / 1_000_000,
                (System.nanoTime() - wall) / 1e6,
                threads.getThreadAllocatedBytes(id) - bytes,
            )
        check(solution.isYes) { "${workload.goal} failed: $solution" }
        return measure
    }

    @Test
    fun engines() {
        val engines =
            (property("engines")?.split(",")?.map { it.trim() } ?: listOf("arg2p", "tuprolog"))
                .associateWith { if (it == "arg2p") ::arg2p else ::tuProlog }
        val output = File(property("output") ?: "build/reports/benchmark/tuprolog.csv")
        output.parentFile.mkdirs()
        val rows = mutableListOf("workload,engine,cpu_ms,wall_ms,mb")
        for ((name, workload) in workloads) {
            for ((engine, solverOf) in engines) {
                val first = run(workload, solverOf)
                val measures = if (first.ms > budget) listOf(first) else List(5) { run(workload, solverOf) }
                val ms = measures.map { it.ms }.sorted()[measures.size / 2]
                val wall = measures.map { it.wall }.sorted()[measures.size / 2]
                val mb = measures.map { it.bytes }.sorted()[measures.size / 2] / 1_048_576.0
                println("TUPROLOG|$name|$engine|${ms}ms|${"%.2f".format(wall)}ms wall|${"%.1f".format(mb)}MB")
                rows += "$name,$engine,$ms,${"%.2f".format(wall)},${"%.1f".format(mb)}"
            }
        }
        output.writeText(rows.joinToString("\n") + "\n")
    }
}
