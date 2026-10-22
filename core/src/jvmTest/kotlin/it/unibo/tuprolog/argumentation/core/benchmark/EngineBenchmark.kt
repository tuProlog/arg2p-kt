package it.unibo.tuprolog.argumentation.core.benchmark

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import java.io.File
import java.lang.management.ManagementFactory
import java.util.concurrent.locks.LockSupport
import kotlin.test.Test

/**
 * Timings of workloads exercising the resolution engine: module calls, resolution over argument terms, graph
 * building, query mode and labellings. Run with `./gradlew :core:jvmBenchmark --tests "*EngineBenchmark"`,
 * optionally passing (as -P project properties):
 * - benchmark.workloads: comma separated subset of the workloads
 * - benchmark.output: CSV report (default build/reports/benchmark/engine.csv)
 * - benchmark.baseline: a previous report to compare against
 * - benchmark.profile: if set, a sampled profile of each workload is written next to the report (JFR cannot walk
 *   the deep stacks of the engine: the stack of the solving thread is sampled instead)
 *
 * Each workload runs once to warm up, then five times on a fresh solver: the median CPU time of the solving thread
 * is reported (the wall-clock time varies too much with the load of the machine).
 */
class EngineBenchmark {
    private fun property(name: String) = System.getProperty("benchmark.$name")?.takeIf { it.isNotBlank() }

    private val config =
        "\ngraphBuildMode(standard_af).\nstatementLabellingMode(statement).\nargumentLabellingMode(grounded)." +
            "\norderingPrinciple(last).\norderingComparator(elitist).\ngraphExtension(standardPref)."

    private class Workload(
        val theory: String,
        val setup: String?,
        val goal: String,
    )

    private fun abstractFramework(
        semantics: String,
        attacks: Set<Pair<Int, Int>>,
        n: Int,
    ): Workload {
        val arguments = (0 until n).joinToString(", ", "[", "]") { "a$it" }
        val pairs = attacks.joinToString(", ", "[", "]") { (a, b) -> "(a$a, a$b)" }
        return Workload(
            "argumentLabellingMode($semantics).",
            null,
            "findall(I, abstract::solve($arguments, $pairs, I, _, _), _)",
        )
    }

    private val workloads: Map<String, Workload> by lazy {
        val theories = LabellingBenchmarkWorker.THEORIES
        val families = LabellingBenchmarkWorker.FAMILIES
        val bigVars = (1..100).joinToString(", ", "[", "]") { "V$it" }
        val argument = "[[r0], r0, [p1(c1)], [p0(c1)], [[r0], [r0], [f1]]]"
        linkedMapOf(
            "modcall-trivial" to Workload("", null, "findall(_, (between(1, 20000, _), utils::true), _)"),
            "modcall-vars" to Workload("", null, "findall(_, (between(1, 2000, _), utils::(X = $bigVars)), _)"),
            "resolution-attackers" to
                Workload(
                    "r0 : p0(X) => p1(X).\nf1 :=> p0(c1).$config",
                    "context_reset, parser:::convertAllRules(_)",
                    "findall(C, (between(1, 2000, _), attack::findPossibleAttackers($argument, C)), _)",
                ),
            "graph-chain" to Workload(theories.getValue("chain")(40) + config, null, "buildLabelSets(_, _, _)"),
            "graph-conflicts" to Workload(theories.getValue("conflicts")(100) + config, null, "buildLabelSets(_, _, _)"),
            "graph-strict" to Workload(theories.getValue("strict")(100) + config, null, "buildLabelSets(_, _, _)"),
            "query-chain" to Workload(theories.getValue("chain")(40) + config + "\nqueryMode.", null, "answerQuery(p8(X), _, _, _)"),
            "preferred-pairs" to abstractFramework("preferred", families.getValue("pairs")(16), 16),
            "complete-pairChain" to abstractFramework("complete", families.getValue("pairChain")(12), 12),
        )
    }

    private fun run(
        workload: Workload,
        profile: File? = null,
    ): Long {
        val solver = solverWithTheory(workload.theory)
        workload.setup?.let { solver.solve(Struct.parse(it, solver.operators)).first() }
        val goal = Struct.parse(workload.goal, solver.operators)
        val sampler = profile?.let { StackSampler(Thread.currentThread()).also { it.start() } }
        // CPU time of the solving thread: the wall-clock time varies too much with the load of the machine
        val threads = ManagementFactory.getThreadMXBean()
        val start = threads.currentThreadCpuTime
        val solution = solver.solve(goal).first()
        val time = (threads.currentThreadCpuTime - start) / 1_000_000
        check(solution.isYes) { "${workload.goal} failed: $solution" }
        sampler?.stopAndReport(profile)
        return time
    }

    @Test
    fun engine() {
        val selected = property("workloads")?.split(",")?.map { it.trim() } ?: workloads.keys.toList()
        val baseline =
            property("baseline")?.let { path ->
                File(path).readLines().drop(1).associate { line -> line.split(",").let { it[0] to it[1].toLong() } }
            } ?: emptyMap()
        val output = File(property("output") ?: "build/reports/benchmark/engine.csv")
        output.parentFile.mkdirs()
        val results =
            selected.map { name ->
                val workload = workloads.getValue(name)
                run(workload)
                val ms = List(5) { run(workload) }.sorted()[2]
                property("profile")?.let { run(workload, File(output.parentFile, "engine-profile-$name.txt")) }
                val comparison = baseline[name]?.let { " (baseline ${it}ms, x${"%.2f".format(it.toDouble() / ms.coerceAtLeast(1))})" } ?: ""
                println("ENGINE|$name|${ms}ms$comparison")
                name to ms
            }
        output.writeText("workload,ms\n" + results.joinToString("\n") { "${it.first},${it.second}" } + "\n")
    }
}

/**
 * Samples the stack of a thread, reporting how often each method is on it (inclusive) and at its top (self), and the
 * hottest caller-callee pairs among the methods of arg2p and tuProlog
 */
private class StackSampler(
    private val target: Thread,
) : Thread() {
    @Volatile private var running = true
    private val inclusive = HashMap<String, Int>()
    private val self = HashMap<String, Int>()
    private val calls = HashMap<String, Int>()
    private var samples = 0

    init {
        isDaemon = true
    }

    private fun StackTraceElement.method() = "${className.substringAfterLast('.')}.$methodName"

    override fun run() {
        while (running) {
            val stack = target.stackTrace
            if (stack.isNotEmpty()) {
                samples++
                self.merge(stack.first().method(), 1, Int::plus)
                stack.map { it.method() }.toSet().forEach { inclusive.merge(it, 1, Int::plus) }
                stack
                    .take(60)
                    .zipWithNext()
                    .filter { (callee, _) -> callee.className.startsWith("it.unibo") }
                    .map { (callee, caller) -> "${caller.method()} -> ${callee.method()}" }
                    .toSet()
                    .forEach { calls.merge(it, 1, Int::plus) }
            }
            LockSupport.parkNanos(200_000)
        }
    }

    fun stopAndReport(file: File) {
        running = false
        join()

        fun section(
            title: String,
            counts: Map<String, Int>,
            limit: Int,
        ) = "-- $title\n" +
            counts.entries
                .sortedByDescending { it.value }
                .take(limit)
                .joinToString("\n") { "${100 * it.value / samples}% ${it.key}" }
        val report = listOf(section("inclusive", inclusive, 150), section("self", self, 50), section("calls", calls, 100))
        file.writeText("samples=$samples\n" + report.joinToString("\n") + "\n")
    }
}
