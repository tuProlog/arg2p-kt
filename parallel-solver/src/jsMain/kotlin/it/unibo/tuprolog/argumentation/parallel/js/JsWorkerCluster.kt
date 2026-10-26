package it.unibo.tuprolog.argumentation.parallel.js

import it.unibo.tuprolog.argumentation.parallel.message.Add
import it.unibo.tuprolog.argumentation.parallel.message.Address
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import it.unibo.tuprolog.argumentation.parallel.message.KbMessage
import it.unibo.tuprolog.argumentation.parallel.message.Load
import it.unibo.tuprolog.argumentation.parallel.message.MessageCodec
import it.unibo.tuprolog.argumentation.parallel.message.RequireEvaluation
import it.unibo.tuprolog.argumentation.parallel.message.Reset
import it.unibo.tuprolog.argumentation.parallel.node.Catalog
import it.unibo.tuprolog.argumentation.parallel.node.Network
import it.unibo.tuprolog.argumentation.parallel.node.Worker
import kotlin.js.JsExport
import kotlin.js.Promise

/**
 * A distributed solver on the threads of Node.js (worker_threads): the catalog on the calling thread, the workers
 * on [threads] threads, each loading this module ([module], this very file by default) and running the workers of
 * the partitions given to it. The theory is split into at most [parts] partitions (by default as many as the
 * threads). The threads keep Node.js running only while an evaluation is pending; [close] stops them.
 */
@JsExport
class JsWorkerCluster(
    private val threads: Int,
    parts: Int? = threads,
    module: String? = null,
) {
    private val network = Network { to, message -> route(to, message) }
    private val catalog = Catalog(network, parts)
    private val pending = mutableMapOf<String, (EvaluationResponse) -> Unit>()
    private var clients = 0
    private val workers: List<dynamic> = List(threads) { spawn(module ?: thisModule()) }

    init {
        referenced(false)
    }

    /** Replaces the theory with [theory], as Prolog text */
    fun load(theory: String) = catalog.receive(Load(theory))

    /** Adds [clause], as Prolog text, to the theory */
    fun add(clause: String) = catalog.receive(Add(clause))

    /** Forgets the theory */
    fun reset() = catalog.receive(Reset)

    /** The arguments for [statement], as Prolog text, by label */
    fun solve(statement: String): Promise<JsEvaluation> =
        Promise { resolve, _ ->
            val client = "client_${clients++}"
            pending[client] = { resolve(JsEvaluation.of(it)) }
            referenced(true)
            catalog.receive(RequireEvaluation(statement, client))
        }

    /** Stops the threads */
    fun close() = workers.forEach { it.terminate() }

    private fun route(
        to: String,
        message: KbMessage,
    ) {
        when {
            to == Address.CATALOG -> catalog.receive(message)
            Address.isWorker(to) -> workers[Address.indexOf(to) % threads].postMessage(arrayOf(to, MessageCodec.encode(message)))
            message is EvaluationResponse -> {
                pending.remove(to)?.invoke(message)
                if (pending.isEmpty()) referenced(false)
            }
        }
    }

    // A thread running startWorkerThread from the module, its messages routed as those of the other nodes
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
        worker.on("message") { data: dynamic -> route(data[0] as String, MessageCodec.decode(data[1] as String)) }
        worker.on("error") { error: dynamic -> console.error(error) }
        return worker
    }

    // The threads keep Node.js running while an evaluation waits for them, not otherwise
    private fun referenced(on: Boolean) = workers.forEach { if (on) it.ref() else it.unref() }
}

/** The loop of a thread of [JsWorkerCluster]: the workers of the partitions it is given, created as their messages come */
@JsExport
fun startWorkerThread() {
    val port = workerThreads().parentPort
    val network = Network { to, message -> port.postMessage(arrayOf(to, MessageCodec.encode(message))) }
    val nodes = mutableMapOf<String, Worker>()
    port.on("message") { data: dynamic ->
        val to = data[0] as String
        // a message whose handling fails is skipped, as an actor resumed on a failure does
        runCatching { nodes.getOrPut(to) { Worker(to, network) }.receive(MessageCodec.decode(data[1] as String)) }
            .onFailure { console.error(it) }
    }
}

private fun workerThreads(): dynamic = js("require('worker_threads')")

// The file of this module, loaded again by the threads
private fun thisModule(): String = js("__filename") as String
