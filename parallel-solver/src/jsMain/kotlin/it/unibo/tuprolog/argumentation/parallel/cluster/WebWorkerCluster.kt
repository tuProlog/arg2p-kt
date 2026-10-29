@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.parallel.cluster

import it.unibo.tuprolog.argumentation.Flags
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
    threads: Int,
    worker: String,
    parts: Int? = null,
    flags: Flags = Flags.defaults(),
    iceServers: Array<dynamic> = emptyArray(),
) : LinkedCluster(
        // the catalog here, every worker on a Web Worker
        Site(SequentialScheduler(), workers = 0, parts = parts, flags = flags, introducible = true),
    ) {
    private val links = WebSocketLinks(site)
    private val rtc = RtcLinks(site, iceServers)

    // The replies of the site, asked for once every Web Worker greeted it
    private val replies = ThreadReplies(threads)
    private val workers: List<Pair<dynamic, PortLink>> = List(threads) { spawn(worker) }

    /**
     * Federates the cluster with the site listening at [url] (ws://host:port), proving it knows [code] if it requires
     * one: rejected if no site answers in a few seconds
     */
    fun connect(
        url: String,
        code: String? = null,
    ): Promise<Unit> = links.connect(url, code)

    /**
     * An invitation to pair with this cluster by hand, with no server between them: a code for another cluster to
     * [accept] (see [RtcLinks.invite]). Each step of a pairing by hand may be given an AbortSignal ([signal]): once it
     * aborts, the pairing is abandoned, its connection closed, and the promise of the step rejected if still pending.
     */
    fun invite(signal: dynamic = null): Promise<String> = rtc.invite(signal)

    /** The reply to the [invitation] of another cluster, for it to [complete] the pairing: abandoned once [signal] aborts */
    fun accept(
        invitation: String,
        signal: dynamic = null,
    ): Promise<String> = rtc.accept(invitation, signal)

    /** The pairing [invite]d completed with the [reply] of the other cluster: once the two are linked; abandoned once [signal] aborts */
    fun complete(
        reply: String,
        signal: dynamic = null,
    ): Promise<Unit> = rtc.complete(reply, signal)

    /** The sites the cluster is federated with, its Web Workers included */
    val peers: Array<String> get() = site.peerSites.toTypedArray()

    /** The arguments for [statement], as Prolog text, by label (rejected if the cluster closes meanwhile) */
    fun solve(statement: String): Promise<EvaluationResponse> =
        replies.replied<EvaluationResponse> { site.solve(statement, it) }.then { answered(it) }

    /** The cluster as a solver of arg2p: any goal, its solutions and their graphs, the answers of queries */
    val solver: AsyncClusterSolver = AsyncClusterSolver(site, replies)

    /** Closes the links to the other sites, and stops the Web Workers: the evaluations pending are rejected */
    fun close() {
        replies.close()
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
        val received = replies.receiver(site, link)
        worker.onmessage = { event: dynamic -> received(event.data as String) }
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
