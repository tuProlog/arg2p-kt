package it.unibo.tuprolog.argumentation.parallel.cluster

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.federation.FederationStats
import it.unibo.tuprolog.argumentation.parallel.federation.RtcLinks
import it.unibo.tuprolog.argumentation.parallel.federation.SequentialScheduler
import it.unibo.tuprolog.argumentation.parallel.federation.Site
import it.unibo.tuprolog.argumentation.parallel.federation.WebSocketLinks
import it.unibo.tuprolog.argumentation.parallel.federation.handled
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import kotlin.js.JsExport
import kotlin.js.Promise

/**
 * A distributed solver in a browser: the catalog on the page, the workers on [threads] Web Workers, each running the
 * script at [worker] (one calling [startWebWorker] with this module loaded) and a site of its own, a delegate of the
 * one of the page (see [Site.delegate]). The theory is split into at most [parts] partitions if given, as many as the
 * workers of the Web Workers and of the sites the cluster is federated with otherwise: it federates with JVM and
 * Node.js sites over WebSockets ([connect]), lending them the workers of its Web Workers, and with the browsers they
 * are linked to over WebRTC (see [RtcLinks], with [iceServers] if given, as RTCPeerConnection takes them). The graph is
 * labelled as the standard solver does with [flags].
 */
@JsExport
class WebWorkerCluster(
    private val threads: Int,
    worker: String,
    parts: Int? = null,
    flags: Flags = Flags.defaults(),
    iceServers: Array<dynamic> = emptyArray(),
) {
    // the catalog here, every worker on a Web Worker
    private val site = Site(SequentialScheduler(), workers = 0, parts = parts, flags = flags, introducible = true)
    private val links = WebSocketLinks(site)
    private val rtc = RtcLinks(site, iceServers)

    // the evaluations asked before every Web Worker greeted the site wait for them
    private val waiting = mutableListOf<() -> Unit>()
    private var greeted = 0
    private val workers: List<Pair<dynamic, PortLink>> = List(threads) { spawn(worker) }

    // The replies of the site, asked for once every Web Worker greeted it
    private val replies =
        object : AsyncReplies {
            override fun <T> replied(ask: ((T) -> Unit) -> Unit): Promise<T> =
                Promise { resolve, _ ->
                    val evaluate = { ask(resolve) }
                    if (greeted == threads) evaluate() else waiting += evaluate
                }
        }

    /** Federates the cluster with the site listening at [url] (ws://host:port) */
    fun connect(url: String): Promise<Unit> = links.connect(url)

    /**
     * An invitation to pair with this cluster by hand, with no server between them: a code for another cluster to
     * [accept] (see [RtcLinks.invite])
     */
    fun invite(): Promise<String> = rtc.invite()

    /** The reply to the [invitation] of another cluster, for it to [complete] the pairing */
    fun accept(invitation: String): Promise<String> = rtc.accept(invitation)

    /** The pairing [invite]d completed with the [reply] of the other cluster: once the two are linked */
    fun complete(reply: String): Promise<Unit> = rtc.complete(reply)

    /** The sites the cluster is federated with, its Web Workers included */
    val peers: Array<String> get() = site.peerSites.toTypedArray()

    /** Called whenever a site joins or leaves the peers of the cluster, its Web Workers included */
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

    /** Closes the links to the other sites, and stops the Web Workers */
    fun close() {
        rtc.close()
        links.close()
        workers.forEach { (worker, link) ->
            worker.terminate()
            site.closed(link)
        }
    }

    // A Web Worker running the module at [url] (an ES module, as this one), linked to the site of the page
    private fun spawn(url: String): Pair<dynamic, PortLink> {
        val constructor = js("Worker")
        val options: dynamic = js("({})")
        options.type = "module"
        val worker = js("new constructor(url, options)")
        val link = PortLink(worker) { worker.terminate() }
        var first = true
        worker.onmessage = { event: dynamic ->
            handled { site.received(link, event.data as String) }
            // its first frame greets the site
            if (first) {
                first = false
                if (++greeted == threads) waiting.toList().also { waiting.clear() }.forEach { it() }
            }
        }
        worker.onerror = { error: dynamic -> console.error(error) }
        site.delegate(link)
        return Pair(worker, link)
    }
}

/** The loop of a Web Worker of [WebWorkerCluster]: a site running a worker for the site of the page that started it */
@JsExport
fun startWebWorker() {
    val scope = js("self")
    val site = Site(SequentialScheduler(), workers = 1)
    val link = PortLink(scope) { scope.close() }
    scope.onmessage = { event: dynamic -> handled { site.received(link, event.data as String) } }
    site.opened(link)
}
