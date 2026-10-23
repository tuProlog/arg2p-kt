package it.unibo.tuprolog.argumentation.core.benchmark

import java.io.File
import java.lang.management.ManagementFactory
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.TimeUnit
import kotlin.concurrent.thread
import kotlin.test.Test

/**
 * Timings of the argument labelling semantics on abstract frameworks of growing size, to measure optimisations.
 * Run with `./gradlew :core:jvmBenchmark`, optionally passing (as -P project properties):
 * - benchmark.semantics, benchmark.families, benchmark.sizes: comma separated subsets of the defaults
 * - benchmark.timeoutMs: time limit of a single case, a series (family and semantics) stops at its first failure
 * - benchmark.repeat: runs of each case in the same worker, reporting the median time (the limit applies to each run)
 * - benchmark.output: CSV report (default build/reports/benchmark/latest.csv)
 * - benchmark.baseline: a previous CSV report to compare against (speedup, and solution counts that differ)
 *
 * Families also include theories ([LabellingBenchmarkWorker.THEORIES]: chain, conflicts, fanin, strict), run through the structured
 * pipeline: their solutions are the arguments, and the size of the graph and the time of each phase are written to
 * a second report (the output with a -phases suffix).
 *
 * The cases run in a separate JVM ([LabellingBenchmarkWorker]), killed and restarted when a case exceeds the
 * time limit: the module calls of the labellings solve without timeout, so a solver timeout cannot stop them.
 */
class LabellingBenchmark {
    private fun property(name: String) = System.getProperty("benchmark.$name")?.takeIf { it.isNotBlank() }

    private fun listProperty(name: String) = property(name)?.split(",")?.map { it.trim() }

    private val semantics = listProperty("semantics") ?: LabellingBenchmarkWorker.SEMANTICS
    private val families = listProperty("families") ?: LabellingBenchmarkWorker.FAMILIES.keys.toList()
    private val sizes = listProperty("sizes")?.map { it.toInt() } ?: listOf(6, 10, 14)
    private val repetitions = property("repeat")?.toInt() ?: 1
    private val timeoutMs = property("timeoutMs")?.toLong() ?: 5_000L
    private val details = mutableListOf<String>()

    private data class Result(
        val family: String,
        val n: Int,
        val semantics: String,
        val status: String,
        val solutions: Int,
        val ms: Long,
        val heapKB: Long = -1,
    ) {
        val key get() = "$family|$n|$semantics"

        fun csv() = "$family,$n,$semantics,$status,$solutions,$ms,$heapKB"

        companion object {
            // reports written before the heap column have six fields
            fun parse(csv: String) =
                csv.split(",").let { fields ->
                    val (family, n, semantics, status, solutions) = fields
                    Result(family, n.toInt(), semantics, status, solutions.toInt(), fields[5].toLong(), fields.getOrNull(6)?.toLong() ?: -1)
                }
        }
    }

    @Test
    fun benchmark() {
        val series = families.flatMap { family -> semantics.map { "$family:$it:${sizes.joinToString(".")}" } }
        val results = mutableListOf<Result>()
        var pending = series
        while (pending.isNotEmpty()) {
            pending = pending.drop(runWorker(pending, results))
        }
        val output = File(property("output") ?: "build/reports/benchmark/latest.csv")
        output.absoluteFile.parentFile.mkdirs()
        output.writeText((listOf("family,n,semantics,status,solutions,ms,heapKB") + results.map { it.csv() }).joinToString("\n"))
        report(results, baseline())
        println("benchmark report written to ${output.absolutePath}")
        if (details.isNotEmpty()) {
            val phases = File(output.absoluteFile.parentFile, output.nameWithoutExtension + "-phases.csv")
            val header = "family,n,semantics,arguments,attacks,convert_ms,arguments_ms,attacks_ms,extensions_ms,labelling_ms,statements_ms"
            phases.writeText((listOf(header) + details).joinToString("\n"))
            (listOf(header) + details).forEach { println("benchmark phases | " + it.replace(",", " | ")) }
            println("benchmark phases written to ${phases.absolutePath}")
        }
    }

    /** Runs the series in a worker JVM until they end or a case times out: returns the number of series done */
    private fun runWorker(
        series: List<String>,
        results: MutableList<Result>,
    ): Int {
        // the classpath goes in an argument file, too long for a Windows command line
        val arguments = File.createTempFile("benchmark", ".args")
        arguments.writeText("-cp \"${System.getProperty("java.class.path").replace('\\', '/')}\"")
        val jvmArgs = ManagementFactory.getRuntimeMXBean().inputArguments.filter { it.startsWith("-Xss") || it.startsWith("-Xmx") }
        val java =
            ProcessHandle
                .current()
                .info()
                .command()
                .get()
        val process =
            ProcessBuilder(
                listOf(java) + jvmArgs +
                    listOf("-Dbenchmark.repeat=$repetitions", "@${arguments.absolutePath}", LabellingBenchmarkWorker::class.java.name) +
                    series,
            ).redirectErrorStream(true)
                .start()
        val lines = LinkedBlockingQueue<String>()
        thread(isDaemon = true) {
            process.inputStream.bufferedReader().useLines { it.forEach(lines::put) }
            lines.put(END)
        }
        var seriesDone = 0
        var current: String? = null
        var started = 0L
        try {
            while (true) {
                val line = lines.poll(200, TimeUnit.MILLISECONDS)
                when {
                    line == END -> {
                        // the worker ended: all done, or crashed in the middle of a case
                        current?.let { report(failed(it, "crash", System.currentTimeMillis() - started), results) }
                        return if (current != null) seriesDone + 1 else maxOf(seriesDone, 1)
                    }
                    line == null -> {
                        if (current != null && System.currentTimeMillis() - started > timeoutMs * repetitions) {
                            report(failed(current, "timeout", timeoutMs), results)
                            return seriesDone + 1
                        }
                    }
                    line.startsWith("START ") -> {
                        current = line.removePrefix("START ")
                        started = System.currentTimeMillis()
                    }
                    line.startsWith("RESULT ") -> {
                        report(Result.parse(line.removePrefix("RESULT ")), results)
                        current = null
                    }
                    line == "SERIES DONE" -> seriesDone++
                    line.startsWith("DETAIL ") -> details += line.removePrefix("DETAIL ")
                    else -> println("benchmark worker: $line")
                }
            }
        } finally {
            process.destroyForcibly().waitFor()
            arguments.delete()
        }
    }

    private fun failed(
        case: String,
        status: String,
        ms: Long,
    ) = case.split(",").let { (family, n, semantics) -> Result(family, n.toInt(), semantics, status, 0, ms) }

    private fun report(
        result: Result,
        results: MutableList<Result>,
    ) {
        results += result
        println("benchmark ${result.csv()}")
    }

    private fun baseline(): Map<String, Result> =
        property("baseline")
            ?.let { File(it) }
            ?.takeIf { it.exists() }
            ?.readLines()
            ?.drop(1)
            ?.associate { line -> Result.parse(line).let { it.key to it } } ?: emptyMap()

    private fun report(
        results: List<Result>,
        baseline: Map<String, Result>,
    ) {
        val header =
            listOf("family", "n", "semantics", "status", "solutions", "ms", "peak heap MB") +
                if (baseline.isEmpty()) emptyList() else listOf("base ms", "speedup", "base peak heap MB", "check")
        val rows =
            results.map { r ->
                val base = baseline[r.key]
                listOf(r.family, "${r.n}", r.semantics, r.status, "${r.solutions}", "${r.ms}", megabytes(r.heapKB)) +
                    when {
                        baseline.isEmpty() -> emptyList()
                        base == null -> listOf("-", "-", "-", "new")
                        else ->
                            listOf(
                                "${base.ms}${if (base.status != "ok") " (${base.status})" else ""}",
                                if (r.ms > 0) "%.1fx".format(base.ms.toDouble() / r.ms) else "-",
                                megabytes(base.heapKB),
                                if (base.status == "ok" && r.status == "ok" && base.solutions != r.solutions) {
                                    "SOLUTIONS ${base.solutions}->${r.solutions}"
                                } else {
                                    ""
                                },
                            )
                    }
            }
        val widths = header.indices.map { i -> (rows.map { it[i] } + header[i]).maxOf { it.length } }
        (listOf(header) + rows).forEach { row ->
            println("benchmark | " + row.mapIndexed { i, c -> c.padEnd(widths[i]) }.joinToString(" | "))
        }
    }

    private fun megabytes(kb: Long) = if (kb < 0) "-" else "%.1f".format(kb / 1024.0)

    private companion object {
        const val END = "\u0000END"
    }
}
