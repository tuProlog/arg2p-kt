@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.parallel.cluster

import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.federation.Beacons
import it.unibo.tuprolog.argumentation.parallel.federation.LanDiscovery
import it.unibo.tuprolog.argumentation.parallel.federation.Link
import it.unibo.tuprolog.argumentation.parallel.federation.NearbySite
import it.unibo.tuprolog.argumentation.parallel.federation.SequentialScheduler
import it.unibo.tuprolog.argumentation.parallel.federation.Site
import it.unibo.tuprolog.argumentation.parallel.federation.WebSocketLinks
import it.unibo.tuprolog.argumentation.parallel.federation.handled
import it.unibo.tuprolog.argumentation.parallel.federation.later
import it.unibo.tuprolog.argumentation.parallel.federation.nodeModule
import it.unibo.tuprolog.argumentation.parallel.federation.thisModule
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import kotlin.js.JsExport
import kotlin.js.Promise

/**
 * A distributed solver on the threads of Node.js (worker_threads): the catalog on the calling thread, the workers on
 * [threads] threads, each loading this module ([module], this very file by default) and running a site of its own,
 * a delegate of the one of the calling thread (see [Site.delegate]), over the messages between threads: the cluster
 * lends the workers of its threads to the sites it is federated with. The theory is split into at most [parts]
 * partitions if given, as many as the workers of the threads and of the sites it is federated with otherwise (see
 * [listen] and [connect]); the graph is labelled as the standard solver does with [flags]. The threads keep Node.js
 * running only while an evaluation is pending; [close] stops them.
 */
@JsExport
class WorkerCluster(
    threads: Int,
    parts: Int? = null,
    flags: Flags = Flags.defaults(),
    module: String? = null,
) : DiscoveringCluster(
        // the catalog here, every worker on a thread
        Site(SequentialScheduler(), workers = 0, parts = parts, flags = flags).apply { name = nodeModule("os").hostname() as String },
    ) {
    private val links = WebSocketLinks(site)
    private var port: Int? = null
    private var discovery: LanDiscovery? = null

    // The replies of the site, asked for once every thread greeted it: the threads keep Node.js running until they come
    private val replies = ThreadReplies(threads) { on -> referenced(on) }
    private val workers: List<dynamic> = List(threads) { spawn(module ?: thisModule()) }

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
     * With [requireCode] (see [requiresCode]), the clusters heard are [nearby] alone, linked to once paired by hand
     * ([pair]), and linked again with no code from then on. The port it listens on, once its beacons start.
     */
    fun discover(
        beaconPort: Int = Beacons.PORT,
        requireCode: Boolean = false,
    ): Promise<Int> =
        Promise { resolve, reject ->
            check(discovery == null) { "Already discovering" }
            if (requireCode) requiresCode = true
            val listening = port?.let { Promise.resolve(it) } ?: listen()
            listening.then({ wsPort ->
                val started = LanDiscovery(site, links, wsPort, beaconPort).also { discovery = it }
                started.start().then({ resolve(wsPort) }, reject)
            }, reject)
        }

    /**
     * Federates the cluster with the site listening at [url] (ws://host:port), proving it knows [code] if it requires
     * one: rejected if no site answers in a few seconds
     */
    fun connect(
        url: String,
        code: String? = null,
    ): Promise<Unit> = links.connect(url, code)

    /** The sites heard on the local network, not linked to this cluster (see [discover]) */
    val nearby: Array<NearbySite> get() = site.nearby.toTypedArray()

    /**
     * Links this cluster to [site], nearby, proving it knows its [code]: the two paired, linked again with no code
     * while they run; rejected if it refuses the code, does not prove it knows it as well (a site claiming the
     * identifier of another), does not answer in a few seconds, or if no site nearby is [site]
     */
    fun pair(
        site: String,
        code: String,
    ): Promise<Unit> =
        Promise { resolve, reject ->
            val nearby = runCatching { nearbySite(site) }.getOrElse { return@Promise reject(it) }
            var opened: Link? = null
            var done = false
            // the link greeted, refused or closed, or not in time: the greeting no longer waited for then
            val settle = { refusal: String? ->
                if (!done) {
                    done = true
                    opened?.let { this.site.forgetGreeting(it) }
                    if (refusal == null) resolve(Unit) else reject(Arg2pException(refusal))
                }
            }
            later(PAIRING) {
                if (!done) opened?.close()
                settle("$site did not answer in time")
            }
            links
                .connect(nearby.address, code, PAIRING) { link ->
                    opened = link
                    this.site.greeting(link) { refusal -> settle(refusal?.let { "$site refused the link: $it" }) }
                }.catch { settle("$site cannot be reached: ${it.message}") }
        }

    /** The sites the cluster is federated with, its threads included */
    val peers: Array<String> get() = site.peerSites.toTypedArray()

    /** The arguments for [statement], as Prolog text, by label (rejected if the cluster closes meanwhile) */
    fun solve(statement: String): Promise<EvaluationResponse> =
        replies.replied<EvaluationResponse> { site.solve(statement, it) }.then { answered(it) }

    /** The cluster as a solver of arg2p: any goal, its solutions and their graphs, the answers of queries */
    val solver: AsyncClusterSolver = AsyncClusterSolver(site, replies)

    /** Closes the links to the other sites, and stops the threads: the evaluations pending are rejected */
    fun close() {
        replies.close()
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
        val received = replies.receiver(site, link)
        worker.on("message") { frame: dynamic -> received(frame as String) }
        worker.on("exit") { _: dynamic -> site.closed(link) }
        worker.on("error") { error: dynamic -> console.error(error) }
        site.delegate(link)
        return worker
    }

    // The threads keep Node.js running while an evaluation waits for them, not otherwise
    private fun referenced(on: Boolean) = workers.forEach { if (on) it.ref() else it.unref() }

    private companion object {
        // How long a pairing waits for the other site, in milliseconds
        const val PAIRING = 10_000
    }
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
