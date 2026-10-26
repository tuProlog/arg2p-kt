package it.unibo.tuprolog.argumentation.parallel.js

import it.unibo.tuprolog.argumentation.parallel.federation.Link
import it.unibo.tuprolog.argumentation.parallel.federation.SequentialScheduler
import it.unibo.tuprolog.argumentation.parallel.federation.Site
import kotlin.js.JsExport
import kotlin.js.Promise

/**
 * A distributed solver on the threads of Node.js (worker_threads): the catalog on the calling thread, the workers on
 * [threads] threads, each loading this module ([module], this very file by default) and running a site of its own,
 * federated with the one of the calling thread over the messages between threads. The theory is split into at most
 * [parts] partitions if given, as many as the threads otherwise. The threads keep Node.js running only while an
 * evaluation is pending; [close] stops them.
 */
@JsExport
class JsWorkerCluster(
    private val threads: Int,
    parts: Int? = null,
    module: String? = null,
) {
    // the catalog here, every worker on a thread
    private val site = Site(SequentialScheduler(), workers = 0, parts = parts)
    private val waiting = mutableListOf<() -> Unit>()
    private var pending = 0
    private val workers: List<dynamic> = List(threads) { spawn(module ?: thisModule()) }

    init {
        referenced(false)
        // the evaluations asked before every thread greeted the site wait for them
        site.onPeersChanged = {
            if (site.peerSites.size >= threads) waiting.toList().also { waiting.clear() }.forEach { it() }
        }
    }

    /** Replaces the theory with [theory], as Prolog text */
    fun load(theory: String) = site.load(theory)

    /** Adds [clause], as Prolog text, to the theory */
    fun add(clause: String) = site.add(clause)

    /** Forgets the theory */
    fun reset() = site.reset()

    /** The arguments for [statement], as Prolog text, by label */
    fun solve(statement: String): Promise<JsEvaluation> =
        Promise { resolve, _ ->
            pending++
            referenced(true)
            val evaluate = {
                site.solve(statement) {
                    resolve(JsEvaluation.of(it))
                    if (--pending == 0) referenced(false)
                }
            }
            if (site.peerSites.size >= threads) evaluate() else waiting += evaluate
        }

    /** Stops the threads */
    fun close() = workers.forEach { it.terminate() }

    // A thread running startWorkerThread from the module, linked to the site of this thread
    private fun spawn(module: String): dynamic {
        val code =
            "const { workerData } = require('worker_threads');\n" +
                "require(workerData.module).it.unibo.tuprolog.argumentation.parallel.js.startWorkerThread();"
        val options: dynamic = js("({})")
        options.eval = true
        options.workerData = js("({})")
        options.workerData.module = module
        val thread = workerThreads().Worker
        val worker = js("new thread(code, options)")
        val link = PortLink(worker) { worker.terminate() }
        worker.on("message") { frame: dynamic -> handled { site.received(link, frame as String) } }
        worker.on("exit") { _: dynamic -> site.closed(link) }
        worker.on("error") { error: dynamic -> console.error(error) }
        site.opened(link)
        return worker
    }

    // The threads keep Node.js running while an evaluation waits for them, not otherwise
    private fun referenced(on: Boolean) = workers.forEach { if (on) it.ref() else it.unref() }
}

/** The loop of a thread of [JsWorkerCluster]: a site running a worker for the site of the thread that started it */
@JsExport
fun startWorkerThread() {
    val port = workerThreads().parentPort
    val site = Site(SequentialScheduler(), workers = 1)
    val link = PortLink(port) { port.close() }
    port.on("message") { frame: dynamic -> handled { site.received(link, frame as String) } }
    site.opened(link)
}

// A link over the messages between two threads of Node.js, on [port] (a Worker, or the port of the parent)
private class PortLink(
    private val port: dynamic,
    private val closing: () -> Unit,
) : Link {
    override fun send(frame: String) {
        port.postMessage(frame)
    }

    override fun close() = closing()
}

// A frame whose handling fails is skipped, as an actor resumed on a failure does
private fun handled(block: () -> Unit) = runCatching(block).onFailure { console.error(it) }

private fun workerThreads(): dynamic = js("require('worker_threads')")

// The file of this module, loaded again by the threads
private fun thisModule(): String = js("__filename") as String
