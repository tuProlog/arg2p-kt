package it.unibo.tuprolog.argumentation.lsp

import it.unibo.tuprolog.argumentation.Arg2p
import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.cluster.ThreadedCluster
import it.unibo.tuprolog.argumentation.parallel.federation.Beacons
import it.unibo.tuprolog.argumentation.parallel.federation.StaticContent
import java.net.DatagramSocket
import java.net.Inet4Address
import java.net.InetAddress
import java.net.NetworkInterface
import java.util.concurrent.CancellationException
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ExecutionException
import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.ReentrantLock

/**
 * The federation of the language server with the others of the local network (the editors of other devices) and the
 * browsers joining it: a cluster of the parallel solver ([ThreadedCluster]), its workers on [threads] threads of the
 * server, which solves the queries of the pages run in parallel (see [Resolution.Federated]).
 *
 * Enabled, the cluster listens on [port] (any free one by default), and its beacons go to [beaconPort] (see
 * [Beacons]): the clusters nearby see each other, and are linked only once paired by the code of one of them ([pair]),
 * linked again with no code while they run. The port it listens on serves the page a browser joins it from (a phone
 * scanning the link of the page, see [FederationState.joinUrl]): the page asks for the code, and lends the workers of
 * its Web Workers. Disabled, the cluster neither listens nor is heard, and solves on the threads of the server alone.
 * [changed] is told the state of the federation whenever it changes.
 */
internal class Federation(
    private val solving: Solving,
    private val beaconPort: Int = Beacons.PORT,
    private val threads: Int = ThreadedCluster.processors,
    private val port: Int = 0,
    private val changed: (FederationState) -> Unit = {},
) : Resolution.Factory,
    AutoCloseable {
    // the cluster, one request of it at a time (a cluster has one theory), and its changes
    private val lock = ReentrantLock()

    @Volatile private var cluster: ThreadedCluster? = null

    // the theory and the flags the cluster holds, as text, if it was given any
    private var loaded: Pair<String, String>? = null

    // the link of the page joining the cluster, while enabled
    @Volatile private var joinUrl: String? = null

    // the name given to the site, if any (see named), and the names of the sites heard
    @Volatile private var name: String? = null
    private val names = ConcurrentHashMap<String, String>()

    /** Whether the federation is enabled: the cluster heard on the local network, linked to the clusters paired */
    @Volatile var enabled: Boolean = false
        private set

    /** What the federation is now */
    val state: FederationState
        get() {
            val running = cluster?.takeIf { enabled }
            val stats = running?.stats
            return FederationState(
                enabled = enabled,
                name = running?.name ?: named(),
                code = running?.code,
                joinUrl = joinUrl.takeIf { running != null },
                workers = threads,
                nearby = running?.nearby.orEmpty().map { NearbyData(it.id, it.name, it.requiresCode) },
                sites =
                    stats
                        ?.sites
                        .orEmpty()
                        .drop(1)
                        .filterNot { it.delegate }
                        .map { SiteData(it.id, names[it.id], it.workers, it.via) },
            )
        }

    /**
     * Enables the federation, or disables it: the cluster made anew, its links closed (a query it solves is stopped);
     * an exception if it cannot listen, the federation disabled
     */
    fun enable(enabled: Boolean) {
        // already so: the query the cluster solves goes on
        if (enabled == this.enabled) return
        exclusively {
            // made so meanwhile, by another call
            if (enabled == this.enabled) return@exclusively
            this.enabled = enabled
            discard()
            try {
                if (enabled) cluster = made()
            } catch (failure: Exception) {
                this.enabled = false
                discard()
                throw failure
            } finally {
                notifyChanged()
            }
        }
    }

    /** Names the site [name], as the sites nearby are told (blank: the name of the host) */
    fun rename(name: String) {
        this.name = name.trim().ifEmpty { null }
        cluster?.name = named()
        notifyChanged()
    }

    /** A new code of the site, the one before refused from now on (the pairings made stay) */
    fun newCode() {
        cluster?.takeIf { enabled }?.newCode()
        notifyChanged()
    }

    /** Pairs the site with [site], nearby, presenting [code]: an [Arg2pException] if it refuses it, or is not there */
    fun pair(
        site: String,
        code: String,
    ) {
        val running = cluster?.takeIf { enabled } ?: throw Arg2pException("The federation is disabled")
        try {
            running.pair(site, code.trim())
        } catch (failure: Arg2pException) {
            throw failure
        } catch (failure: Exception) {
            // no site at its address any more, or no answer in time
            throw Arg2pException(failure.message ?: "$site did not answer")
        } finally {
            notifyChanged()
        }
    }

    override fun resolve(
        theory: String,
        flags: Flags,
        query: String,
        profile: String,
        output: (String) -> Unit,
    ): Resolution = Resolution.Federated(this, theory, flags, query, profile, output)

    /** Closes the links of the cluster, and stops it */
    override fun close() = exclusively { discard() }

    // [change] of the cluster, holding its lock: the query it solves, holding it meanwhile, stopped until it lets it go
    private fun exclusively(change: () -> Unit) {
        while (!lock.tryLock()) {
            cluster?.solver?.stop()
            if (lock.tryLock(STOPPING, TimeUnit.MILLISECONDS)) break
        }
        try {
            change()
        } finally {
            lock.unlock()
        }
    }

    /**
     * [work] on the cluster holding [theory] with [flags], one request at a time, what its solver prints going to
     * [output]: the theory and the flags given again if another request changed them since (the solutions given are
     * then computed again, and left out). Waited for on a thread of its own: an interruption of the thread asking stops
     * it, a CancellationException ([Resolution.STOPPED]).
     */
    fun <T> request(
        theory: String,
        flags: Flags,
        output: (String) -> Unit,
        work: (ThreadedCluster) -> T,
    ): T {
        lock.lockInterruptibly()
        try {
            val running = cluster ?: made().also { cluster = it }
            val asked =
                solving.submit {
                    val given = theory to flags.toString()
                    if (loaded != given) {
                        if (loaded?.second != given.second) running.flags = flags
                        running.load(theory)
                        loaded = given
                    }
                    running.solver.output = output
                    work(running)
                }
            return try {
                asked.get()
            } catch (failure: ExecutionException) {
                throw failure.cause ?: failure
            } catch (_: InterruptedException) {
                // stopped once asked: it ends at once, as stopped
                while (!asked.isDone) {
                    running.solver.stop()
                    runCatching { asked.get(STOPPING, TimeUnit.MILLISECONDS) }
                }
                throw CancellationException(Resolution.STOPPED)
            }
        } finally {
            lock.unlock()
        }
    }

    // A cluster, listening and heard if the federation is enabled, serving the page joining it
    private fun made(): ThreadedCluster {
        val made = ThreadedCluster(threads)
        try {
            made.name = named()
            made.onPeersChanged = ::notifyChanged
            made.onNearbyChanged = {
                made.nearby.forEach { names[it.id] = it.name }
                notifyChanged()
            }
            if (enabled) {
                // the code required before the first link
                made.requiresCode = true
                made.serve(::page)
                val listened = made.listen(port)
                made.discover(beaconPort, requireCode = true)
                joinUrl = lanAddress()?.let { "http://$it:$listened/" }
            }
            return made
        } catch (failure: Exception) {
            made.close()
            throw failure
        }
    }

    // The cluster closed, and forgotten
    private fun discard() {
        cluster?.close()
        cluster = null
        loaded = null
        joinUrl = null
    }

    private fun notifyChanged() = changed(state)

    // The name of the site: the one given, or else the name of the host, as the cluster names its site by default
    private fun named(): String = name ?: ThreadedCluster.HOST_NAME

    companion object {
        /** Where the page joining a cluster loads arg2p from: the npm package of the version of the server */
        val MODULE = "https://cdn.jsdelivr.net/npm/@tuprolog/arg2p@${Arg2p.version()}/arg2p.mjs"

        // How long a request stopped is waited for before it is stopped again, in milliseconds
        private const val STOPPING = 200L

        // An address no packet is sent to (TEST-NET-1): the route to it is the one out of this host
        private const val OUTSIDE = "192.0.2.1"

        // The names of the interfaces of virtual machines and containers, which a phone does not reach
        private val VIRTUAL = Regex("virtual|vethernet|vmware|vbox|hyper-v|docker|wsl|tailscale|zerotier", RegexOption.IGNORE_CASE)

        // The page joining a cluster, and the script of its Web Workers, with the module of arg2p they load
        private val pages: Map<String, StaticContent> by lazy {
            mapOf(
                "/" to StaticContent(Pages.render("join.html", mapOf("module" to MODULE)).toByteArray(), "text/html; charset=utf-8"),
                "/worker.js" to
                    StaticContent(
                        "import { startWebWorker } from '$MODULE';\nstartWebWorker();\n".toByteArray(),
                        "text/javascript; charset=utf-8",
                    ),
            )
        }

        // The page served at [path] (its query left out): the page joining the cluster, or the script of its workers
        private fun page(path: String): StaticContent? = pages[path.substringBefore('?')]

        // The address of this host on the local network, as a phone on it reaches it: that of the route out of the host
        // (opened without sending anything), or else the first one of an interface up that is not virtual; none if there
        // is no site-local IPv4 address
        private fun lanAddress(): String? {
            val routed =
                runCatching {
                    DatagramSocket().use {
                        it.connect(InetAddress.getByName(OUTSIDE), 9)
                        it.localAddress
                    }
                }.getOrNull()
            if (routed is Inet4Address && routed.isSiteLocalAddress) return routed.hostAddress
            return NetworkInterface
                .networkInterfaces()
                .toList()
                .filter {
                    runCatching {
                        it.isUp &&
                            !it.isLoopback &&
                            !it.isVirtual &&
                            !VIRTUAL.containsMatchIn(
                                it.displayName,
                            )
                    }.getOrDefault(false)
                }.flatMap { it.inetAddresses().toList() }
                .firstOrNull { it is Inet4Address && it.isSiteLocalAddress }
                ?.hostAddress
        }
    }
}

/**
 * The federation as the editor shows it: whether it is [enabled], the [name] of this site and its [code] (while
 * enabled), the link of the page a phone joins it from ([joinUrl], none without an address on the local network), the
 * [workers] of the site, the sites heard [nearby] (not linked yet) and the [sites] it is federated with
 */
internal data class FederationState(
    val enabled: Boolean,
    val name: String,
    val code: String?,
    val joinUrl: String?,
    val workers: Int,
    val nearby: List<NearbyData>,
    val sites: List<SiteData>,
)

/** A site heard on the local network: whether it requires a code to link to it */
internal data class NearbyData(
    val id: String,
    val name: String,
    val requiresCode: Boolean,
)

/**
 * A site of the federation: its [name] if it was heard nearby (none for a browser), its [workers] (none if not known),
 * and the site it is reached through ([via]: none for the sites linked to this one)
 */
internal data class SiteData(
    val id: String,
    val name: String?,
    val workers: Int?,
    val via: String?,
)
