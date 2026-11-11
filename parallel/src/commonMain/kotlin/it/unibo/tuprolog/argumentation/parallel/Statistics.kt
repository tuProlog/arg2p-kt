@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.parallel.message.TextReader
import it.unibo.tuprolog.argumentation.parallel.message.TextWriter
import kotlin.js.JsExport
import kotlin.js.JsName

/**
 * An evaluation answered by the client solver (a statement evaluated, or a goal labelling or reading a graph): the
 * part of the graph it was answered on ([locality]: directional or connected in query mode, wholeGraph otherwise, or
 * alone for a graph the solver builds alone, a theory not split or an abstract framework), the arguments and the
 * attacks of the graph labelled (those derived from the others included), the arguments the consumers built for it
 * ([added]: all of them for a graph built anew, none for an evaluation answered on the graph as it was), what each
 * consumer of the graph built ([workers], one for each partition of the theory, the solver the first), and how long the
 * evaluation took, in milliseconds (the construction of the graph included)
 */
@JsExport
data class EvaluationStats(
    val locality: String,
    val arguments: Int,
    val attacks: Int,
    val added: Int,
    @JsExport.Ignore @JsName("workersList") val workers: List<WorkerStats>,
    val milliseconds: Double,
) {
    /** The consumers of the graph, by partition (an array in JavaScript) */
    @JsName("workers")
    val workerArray: Array<WorkerStats> get() = workers.toTypedArray()
}

/**
 * A consumer of a graph: its [partition] of the theory, the [site] that ran it (the node of the parallel solver, or a host it was
 * placed on), the arguments of its rules it built and the attacks it saved
 */
@JsExport
data class WorkerStats(
    val partition: Int,
    val site: String,
    val arguments: Int,
    val attacks: Int,
)

/** The statistics of an evaluation as text, for a solver on another thread than its parallel solver */
internal object StatsCodec {
    fun encode(stats: EvaluationStats): String =
        TextWriter()
            .text(stats.locality)
            .number(stats.arguments)
            .number(stats.attacks)
            .number(stats.added)
            .list(stats.workers) { number(it.partition).text(it.site).number(it.arguments).number(it.attacks) }
            .text(stats.milliseconds.toString())
            .toString()

    fun decodeEvaluation(text: String): EvaluationStats =
        TextReader(text).run {
            val locality = text()
            val arguments = number()
            val attacks = number()
            val added = number()
            val workers = list { WorkerStats(number(), text(), number(), number()) }
            EvaluationStats(locality, arguments, attacks, added, workers, text().toDouble())
        }
}
