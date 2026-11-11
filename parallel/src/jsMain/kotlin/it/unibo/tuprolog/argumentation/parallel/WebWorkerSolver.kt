@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.ExperimentalArg2pApi
import it.unibo.tuprolog.argumentation.Flags
import kotlin.js.JsExport

/**
 * A distributed solver in a browser, nothing of it on the page but this object: [threads] consumers on as many Web Workers,
 * one more for its node, each running the script at [worker] (one calling [startWebWorker] with this module loaded). The
 * theory is split into at most [parts] partitions if given, as many as the consumers it can run otherwise; the graph is
 * labelled as the standard solver does with [flags].
 *
 * The Web Workers wait for one another on a SharedArrayBuffer, which a browser gives only to pages isolated across origins
 * (served with Cross-Origin-Opener-Policy: same-origin and Cross-Origin-Embedder-Policy: require-corp, over HTTPS or on
 * localhost). On any other page the parallel solver solves alone, on the Web Worker of its node.
 */
@JsExport
@ExperimentalArg2pApi
class WebWorkerSolver(
    threads: Int,
    worker: String,
    parts: Int? = null,
    flags: Flags = Flags.defaults(),
) : AsyncParallelSolver(NodeProxy.spawn(threads, parts, flags, worker))
