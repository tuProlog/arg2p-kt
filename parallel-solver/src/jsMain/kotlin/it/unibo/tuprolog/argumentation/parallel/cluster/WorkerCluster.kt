package it.unibo.tuprolog.argumentation.parallel.cluster

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.federation.Beacons
import it.unibo.tuprolog.argumentation.parallel.federation.FederationStats
import it.unibo.tuprolog.argumentation.parallel.federation.LanDiscovery
import it.unibo.tuprolog.argumentation.parallel.federation.Link
import it.unibo.tuprolog.argumentation.parallel.federation.SequentialScheduler
import it.unibo.tuprolog.argumentation.parallel.federation.Site
import it.unibo.tuprolog.argumentation.parallel.federation.WebSocketLinks
import it.unibo.tuprolog.argumentation.parallel.federation.handled
import it.unibo.tuprolog.argumentation.parallel.federation.nodeModule
import it.unibo.tuprolog.argumentation.parallel.federation.thisModule
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import kotlin.js.JsExport
import kotlin.js.Promise

/**
 * A distributed solver on the threads of Node.js (worker_threads): the catalog on the calling thread, the workers on
 * [threads] threads, each loading this module ([module], this very file by default) and running a site of its own,
 * a delegate of the one of the calling thread (see [Site.delegate]), over the messages between threads: the cluster
 * lends the workers of its threads to the sites it is federated with. The theory is split into at most
 * [parts] partitions if given, as many as the workers of the threads and of the sites it is federated with otherwise
 * (see [listen] and [connect]); the graph is labelled as the standard solver does with [flags]. The threads keep Node.js running only while an evaluation is pending; [close] stops
 * them.
 */
@JsExport
class WorkerCluster(
    private val threads: Int,
    parts: Int? = null,
    flags: Flags = Flags.defaults(),
    module: String? = null,
) {
    // the catalog here, every worker on a thread
    private val site = Site(SequentialScheduler(), workers = 0, parts = parts, flags = flags)
    private val links = WebSocketLinks(site)
    private var port: Int? = null
    private var discovery: LanDiscovery? = null

    // the evaluations asked before every thread greeted the site wait for them
    private val waiting = mutableListOf<() -> Unit>()
    private var greeted = 0
    private var pending = 0
    private val workers: List<dynamic> = List(threads) { spawn(module ?: thisModule()) }

    // The replies of the site, asked for once every thread greeted it: the threads keep Node.js running until they come
    private val replies =
        object : AsyncReplies {
            override fun <T> replied(ask: ((T) -> Unit) -> Unit): Promise<T> =
                Promise { resolve, _ ->
                    pending++
                    referenced(true)
                    val evaluate = {
                        ask {
                            resolve(it)
                            if (--pending == 0) referenced(false)
                        }
                    }
                    if (greeted == threads) evaluate() else waiting += evaluate
                }
        }

    init {
        referenced(false)
    }

    /** Accepts the links of other sites (on any platform) over WebSockets, on [port] (an ephemeral port for 0): the port taken */
    fun listen(port: Int = 0): Promise<Int> =
        links.listen(port).then {
            this.port = it
            it
        }

    /**
     * Federates the cluster with the clusters of the local network that discover the others as well, as they come
     * (see [Beacons]): it listens (on an ephemeral port, if it did not already), and its beacons go to [beaconPort].
     * The port it listens on, once its beacons start.
     */
    fun discover(beaconPort: Int = Beacons.PORT): Promise<Int> =
        Promise { resolve, reject ->
            check(discovery == null) { "Already discovering" }
            val listening = port?.let { Promise.resolve(it) } ?: listen()
            listening.then({ wsPort ->
                val started = LanDiscovery(site, links, wsPort, beaconPort).also { discovery = it }
                started.start().then({ resolve(wsPort) }, reject)
            }, reject)
        }

    /** Federates the cluster with the site listening at [url] (ws://host:port) */
    fun connect(url: String): Promise<Unit> = links.connect(url)

    /** The sites the cluster is federated with, its threads included */
    val peers: Array<String> get() = site.peerSites.toTypedArray()

    /** Called whenever a site joins or leaves the peers of the cluster, its threads included */
    var onPeersChanged: (() -> Unit)?
        get() = site.onPeersChanged
        set(value) {
            site.onPeersChanged = value
        }

    /** What the federation does, as the cluster knows it: the sites, and the last evaluation it answered (see [Site.stats]) */
    val stats: FederationStats get() = site.stats

    /**
     * The flags of the solver, the initial ones those given: those set apply from the next evaluation on, the graph
     * built anew, the links and the peers of the cluster kept
     */
    var flags: Flags
        get() = site.flags
        set(value) {
            site.flags = value
        }

    /** Replaces the theory with [theory], as Prolog text */
    fun load(theory: String) = site.load(theory)

    /** Adds [clause], as Prolog text, to the theory */
    fun add(clause: String) = site.add(clause)

    /** Forgets the theory */
    fun reset() = site.reset()

    /** The arguments for [statement], as Prolog text, by label */
    fun solve(statement: String): Promise<EvaluationResponse> =
        replies.replied<EvaluationResponse> { site.solve(statement, it) }.then { answered(it) }

    /** The cluster as a solver of arg2p: any goal, its solutions and their graphs, the answers of queries */
    val solver: AsyncClusterSolver = AsyncClusterSolver(site, replies)

    /** Closes the links to the other sites, and stops the threads */
    fun close() {
        discovery?.close()
        links.close()
        workers.forEach { it.terminate() }
    }

    // A thread running startWorkerThread from the module, linked to the site of this thread
    private fun spawn(module: String): dynamic {
        val code =
            "const { workerData } = require('worker_threads');\n" +
                "import(workerData.module).then(module => module.startWorkerThread());"
        val options: dynamic = js("({})")
        options.eval = true
        options.workerData = js("({})")
        options.workerData.module = module
        val thread = workerThreads().Worker
        val worker = js("new thread(code, options)")
        val link = PortLink(worker) { worker.terminate() }
        var first = true
        worker.on("message") { frame: dynamic ->
            handled { site.received(link, frame as String) }
            // its first frame greets the site
            if (first) {
                first = false
                if (++greeted == threads) waiting.toList().also { waiting.clear() }.forEach { it() }
            }
        }
        worker.on("exit") { _: dynamic -> site.closed(link) }
        worker.on("error") { error: dynamic -> console.error(error) }
        site.delegate(link)
        return worker
    }

    // The threads keep Node.js running while an evaluation waits for them, not otherwise
    private fun referenced(on: Boolean) = workers.forEach { if (on) it.ref() else it.unref() }
}

/** The loop of a thread of [WorkerCluster]: a site running a worker for the site of the thread that started it */
@JsExport
fun startWorkerThread() {
    val port = workerThreads().parentPort
    val site = Site(SequentialScheduler(), workers = 1)
    val link = PortLink(port) { port.close() }
    port.on("message") { frame: dynamic -> handled { site.received(link, frame as String) } }
    site.opened(link)
}

// A link over the messages between two threads of Node.js, on [port] (a Worker, or the port of the parent)
internal class PortLink(
    private val port: dynamic,
    private val closing: () -> Unit,
) : Link {
    override fun send(frame: String) {
        port.postMessage(frame)
    }

    override fun close() = closing()
}

private fun workerThreads(): dynamic = nodeModule("worker_threads")
