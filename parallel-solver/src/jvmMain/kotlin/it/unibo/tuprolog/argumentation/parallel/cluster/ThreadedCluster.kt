package it.unibo.tuprolog.argumentation.parallel.cluster

import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.federation.Beacons
import it.unibo.tuprolog.argumentation.parallel.federation.FederationStats
import it.unibo.tuprolog.argumentation.parallel.federation.LanDiscovery
import it.unibo.tuprolog.argumentation.parallel.federation.NearbySite
import it.unibo.tuprolog.argumentation.parallel.federation.PoolScheduler
import it.unibo.tuprolog.argumentation.parallel.federation.Site
import it.unibo.tuprolog.argumentation.parallel.federation.StaticContent
import it.unibo.tuprolog.argumentation.parallel.federation.WebSocketLinks
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import java.net.InetAddress
import java.util.concurrent.CompletableFuture
import java.util.concurrent.TimeUnit

/**
 * A distributed solver on the threads of this JVM: each node (the catalog, each worker) handles its messages one
 * at a time, the nodes in parallel on [threads] threads. The theory is split into at most [parts] partitions if
 * given, as many as the workers of the cluster and of the sites it is federated with otherwise (see [site]); the
 * graph is labelled as the standard solver does with [flags]. The nodes share the terms of their messages; with
 * [textual], every message is written as text by its sender and read by its receiver instead, as between processes
 * (see [it.unibo.tuprolog.argumentation.parallel.message.MessageCodec]).
 */
class ThreadedCluster
    @JvmOverloads
    constructor(
        threads: Int = processors,
        parts: Int? = null,
        flags: Flags = Flags.defaults(),
        textual: Boolean = false,
        lends: Boolean = true,
    ) : AutoCloseable {
        private val scheduler = PoolScheduler(threads)

        /** The site of the cluster, running a worker on each thread: to federate it with others, through its links */
        val site = Site(scheduler, threads, parts, flags, lends, textual).apply { name = hostName() }

        // made when the cluster first listens or connects
        private var links: WebSocketLinks? = null
        private var port: Int? = null
        private var discovery: LanDiscovery? = null

        private fun links() = links ?: WebSocketLinks(site).also { links = it }

        // The replies of the site, waited for: the evaluations of several callers run in parallel
        private val replies =
            object : Replies {
                override fun <T> replied(ask: ((T) -> Unit) -> Unit): T =
                    CompletableFuture<T>().also { response -> ask { response.complete(it) } }.join()
            }

        /** The identifier of the site of the cluster, as the others know it */
        val id: String get() = site.id

        /** The sites the cluster is federated with */
        val peers: Set<String> get() = site.peerSites

        /** Called whenever a site joins or leaves the peers of the cluster, on the thread that saw it */
        var onPeersChanged: (() -> Unit)?
            get() = site.onPeersChanged
            set(value) {
                site.onPeersChanged = value
            }

        /** What the federation does, as the cluster knows it: the sites, and the last evaluation it answered (see [Site.stats]) */
        val stats: FederationStats get() = site.stats

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

        /** Federates the cluster with the site listening at [uri] (ws://host:port), presenting [code] if it requires one */
        @JvmOverloads
        fun connect(
            uri: String,
            code: String? = null,
        ) = links().connect(uri, code = code)

        /** The name of the site of the cluster, as its beacons tell the sites nearby: the name of its host by default */
        var name: String
            get() = site.name
            set(value) {
                site.name = value
            }

        /**
         * Whether the sites linking to this cluster must present its [code], or the token of a pairing made with it:
         * refused otherwise, and after a few wrong codes from an address its codes are refused for a while
         */
        var requiresCode: Boolean
            get() = site.requiresCode
            set(value) {
                site.requiresCode = value
            }

        /** The code the sites linking to this cluster present, if it requires one: four digits, random */
        val code: String get() = site.code

        /** A new [code], the one before refused from now on (the pairings made stay) */
        fun newCode(): String = site.newCode()

        /** The sites heard on the local network, not linked to this cluster (see [discover]) */
        val nearby: List<NearbySite> get() = site.nearby

        /** Called whenever the sites heard on the local network change */
        var onNearbyChanged: (() -> Unit)?
            get() = site.onNearbyChanged
            set(value) {
                site.onNearbyChanged = value
            }

        /**
         * Links this cluster to [site], nearby, presenting [code]: the two paired, linked again with no code while they
         * run; an Arg2pException if it refuses the code (or no site nearby is [site])
         */
        fun pair(
            site: String,
            code: String,
        ) {
            val nearby = this.site.nearby.find { it.id == site } ?: throw Arg2pException("No site $site nearby")
            val greeted = CompletableFuture<String?>()
            links().connect(nearby.address, code = code) { this.site.greeting(it) { refusal -> greeted.complete(refusal) } }
            greeted.get(PAIRING, TimeUnit.MILLISECONDS)?.let { throw Arg2pException("$site refused the link: $it") }
        }

        /**
         * Serves [pages] over plain HTTP on the port the cluster listens on (see [WebSocketLinks.pages]): the page a GET
         * of a path is answered with, none (null) for a 404
         */
        fun serve(pages: (path: String) -> StaticContent?) {
            links().pages = pages
        }

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

        /** The arguments for [statement], as Prolog text, by label: the evaluations of several callers run in parallel */
        fun solve(statement: String): EvaluationResponse = answered(replies.replied { site.solve(statement, it) })

        /**
         * The cluster as a solver of arg2p: any goal, its solutions and their graphs, the answers of queries. The goals
         * of several callers are solved in turn by the catalog, each solved again from its start when another one was
         * solved since its last solution.
         */
        val solver: ClusterSolver = ClusterSolver(site, replies)

        companion object {
            /** The threads of a cluster by default: as many as the processors */
            @JvmStatic
            val processors: Int get() = Runtime.getRuntime().availableProcessors()

            // How long a pairing waits for the other site, in milliseconds
            private const val PAIRING = 10_000L

            // The name of this host, if it has one
            private fun hostName(): String = runCatching { InetAddress.getLocalHost().hostName }.getOrDefault("")
        }

        /** Closes the links to the other sites, and stops the threads */
        override fun close() {
            discovery?.close()
            links?.close()
            scheduler.close()
        }
    }
