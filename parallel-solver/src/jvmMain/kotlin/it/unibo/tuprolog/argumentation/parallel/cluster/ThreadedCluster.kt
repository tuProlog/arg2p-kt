package it.unibo.tuprolog.argumentation.parallel.cluster

import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.federation.Beacons
import it.unibo.tuprolog.argumentation.parallel.federation.LanDiscovery
import it.unibo.tuprolog.argumentation.parallel.federation.Link
import it.unibo.tuprolog.argumentation.parallel.federation.NearbySite
import it.unibo.tuprolog.argumentation.parallel.federation.PoolScheduler
import it.unibo.tuprolog.argumentation.parallel.federation.Site
import it.unibo.tuprolog.argumentation.parallel.federation.StaticContent
import it.unibo.tuprolog.argumentation.parallel.federation.WebSocketLinks
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import java.net.InetAddress
import java.util.concurrent.CompletableFuture
import java.util.concurrent.CompletionException
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeoutException

/**
 * A distributed solver on the threads of this JVM: each node (the catalog, each worker) handles its messages one
 * at a time, the nodes in parallel on [threads] threads. The theory is split into at most [parts] partitions if
 * given, as many as the workers of the cluster and of the sites it is federated with otherwise (see [site]); the
 * graph is labelled as the standard solver does with [flags]. The nodes share the terms of their messages; with
 * [textual], every message is written as text by its sender and read by its receiver instead, as between processes
 * (see [it.unibo.tuprolog.argumentation.parallel.message.MessageCodec]).
 */
class ThreadedCluster private constructor(
    private val scheduler: PoolScheduler,
    site: Site,
) : DiscoveringCluster(site),
    AutoCloseable {
    @JvmOverloads
    constructor(
        threads: Int = processors,
        parts: Int? = null,
        flags: Flags = Flags.defaults(),
        textual: Boolean = false,
        lends: Boolean = true,
    ) : this(PoolScheduler(threads), threads, parts, flags, textual, lends)

    // the site running a worker on each thread
    private constructor(
        scheduler: PoolScheduler,
        threads: Int,
        parts: Int?,
        flags: Flags,
        textual: Boolean,
        lends: Boolean,
    ) : this(scheduler, Site(scheduler, threads, parts, flags, lends, textual).apply { name = hostName() })

    // made when the cluster first listens or connects
    private var links: WebSocketLinks? = null
    private var port: Int? = null
    private var discovery: LanDiscovery? = null

    private fun links() = links ?: WebSocketLinks(site).also { links = it }

    // The replies of the site, waited for: the evaluations of several callers run in parallel, those waiting when the
    // cluster closes failing then
    private val replies = Waited()

    /** The identifier of the site of the cluster, as the others know it */
    val id: String get() = site.id

    /** The sites the cluster is federated with */
    val peers: Set<String> get() = site.peerSites

    /**
     * Accepts the links of other sites (on any platform) over WebSockets, on [port] of [host] (an ephemeral port for
     * 0): the port taken
     */
    @JvmOverloads
    fun listen(
        port: Int = 0,
        host: String = "0.0.0.0",
    ): Int = links().listen(port, host).also { this.port = it }

    /**
     * Federates the cluster with the clusters of the local network that discover the others as well, as they come
     * (see [Beacons]): it listens (on an ephemeral port, if it did not already), and its beacons go to [beaconPort].
     * With [requireCode] (see [requiresCode]), the clusters heard are [nearby] alone, linked to once paired by hand
     * ([pair]), and linked again with no code from then on.
     */
    @JvmOverloads
    fun discover(
        beaconPort: Int = Beacons.PORT,
        requireCode: Boolean = false,
    ) {
        check(discovery == null) { "Already discovering" }
        if (requireCode) requiresCode = true
        discovery = LanDiscovery(site, links(), port ?: listen(), beaconPort)
    }

    /**
     * Federates the cluster with the site listening at [uri] (ws://host:port), proving it knows [code] if it requires
     * one: an IllegalStateException if no site answers in a few seconds
     */
    @JvmOverloads
    fun connect(
        uri: String,
        code: String? = null,
    ) = links().connect(uri, code = code)

    /** The sites heard on the local network, not linked to this cluster (see [discover]) */
    val nearby: List<NearbySite> get() = site.nearby

    /**
     * Links this cluster to [site], nearby, proving it knows its [code]: the two paired, linked again with no code
     * while they run; an Arg2pException if it refuses the code, does not prove it knows it as well (a site claiming
     * the identifier of another), does not answer in a few seconds, or if no site nearby is [site]
     */
    fun pair(
        site: String,
        code: String,
    ) {
        val nearby = nearbySite(site)
        val greeted = CompletableFuture<String?>()
        var opened: Link? = null
        try {
            links().connect(nearby.address, PAIRING, code) { link ->
                opened = link
                this.site.greeting(link) { refusal -> greeted.complete(refusal) }
            }
            greeted.get(PAIRING, TimeUnit.MILLISECONDS)?.let { throw Arg2pException("$site refused the link: $it") }
        } catch (_: TimeoutException) {
            opened?.close()
            throw Arg2pException("$site did not answer in time")
        } catch (failure: IllegalStateException) {
            throw Arg2pException("$site cannot be reached: ${failure.message}")
        } finally {
            opened?.let { this.site.forgetGreeting(it) }
        }
    }

    /**
     * Serves [pages] over plain HTTP on the port the cluster listens on (see [WebSocketLinks.pages]): the page a GET
     * of a path is answered with, none (null) for a 404
     */
    fun serve(pages: (path: String) -> StaticContent?) {
        links().pages = pages
    }

    /**
     * The arguments for [statement], as Prolog text, by label: the evaluations of several callers run in parallel (an
     * Arg2pException if the cluster closes meanwhile)
     */
    fun solve(statement: String): EvaluationResponse = answered(replies.replied { site.solve(statement, it) })

    /**
     * The cluster as a solver of arg2p: any goal, its solutions and their graphs, the answers of queries. The goals
     * of several callers are solved in turn by the catalog, each solved again from its start when another one was
     * solved since its last solution.
     */
    val solver: ClusterSolver = ClusterSolver(site, replies)

    /** Closes the links to the other sites, and stops the threads: the evaluations waiting fail */
    override fun close() {
        replies.close()
        discovery?.close()
        links?.close()
        scheduler.close()
    }

    // The replies of the site, each waited for by the thread asking: those waiting when the cluster closes, and those
    // asked for since, an Arg2pException
    private class Waited : Replies {
        private val waiting = ConcurrentHashMap.newKeySet<CompletableFuture<*>>()

        @Volatile private var closed = false

        override fun <T> replied(ask: ((T) -> Unit) -> Unit): T {
            val response = CompletableFuture<T>()
            waiting += response
            try {
                if (closed) throw CLOSED
                ask { response.complete(it) }
                return response.join()
            } catch (failure: CompletionException) {
                throw failure.cause ?: failure
            } finally {
                waiting -= response
            }
        }

        fun close() {
            closed = true
            waiting.forEach { it.completeExceptionally(CLOSED) }
        }
    }

    companion object {
        /** The threads of a cluster by default: as many as the processors */
        @JvmStatic
        val processors: Int get() = Runtime.getRuntime().availableProcessors()

        // How long a pairing waits for the other site, in milliseconds
        private const val PAIRING = 10_000L

        private val CLOSED get() = Arg2pException("The cluster is closed")

        // The name of this host, if it has one
        private fun hostName(): String = runCatching { InetAddress.getLocalHost().hostName }.getOrDefault("")
    }
}
