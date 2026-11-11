package it.unibo.tuprolog.argumentation.lsp

import it.unibo.tuprolog.argumentation.Arg2p
import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.federation.SelfSigned
import it.unibo.tuprolog.argumentation.federation.StaticContent
import it.unibo.tuprolog.argumentation.parallel.ThreadedSolver
import java.net.DatagramSocket
import java.net.Inet4Address
import java.net.InetAddress
import java.net.NetworkInterface
import java.util.concurrent.CancellationException
import java.util.concurrent.ExecutionException
import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.ReentrantLock
import it.unibo.tuprolog.argumentation.federation.Federation as ParallelFederation

/**
 * The federation of the language server with the others of the local network (the editors of other devices) and the
 * browsers joining it: a parallel solver of the parallel solver ([ThreadedSolver]), on [threads] threads of the server, which
 * solves the queries of the pages run in parallel (see [Resolution.Federated]), federated while enabled.
 *
 * Enabled, the parallel solver listens on [port] (any free one by default), and links to another by its address and its code ([connect]),
 * linked again with no code while they run. The port it listens on serves the page a browser joins it from (a phone
 * scanning the link of the page, see [FederationState.joinUrl]): the page asks for the code, and lends its Web Workers.
 * Disabled, the parallel solver neither listens nor is heard, and solves on the threads of the server alone.
 * [changed] is told the state of the federation whenever it changes.
 */
internal class Federation(
    private val solving: Solving,
    private val threads: Int = ThreadedSolver.processors,
    private val port: Int = 0,
    private val changed: (FederationState) -> Unit = {},
) : Resolution.Factory,
    AutoCloseable {
    // the parallel solver, one request of it at a time (a parallel solver has one theory), and its changes
    private val lock = ReentrantLock()

    @Volatile private var parallel: ThreadedSolver? = null

    // what federates it, while enabled
    @Volatile private var links: ParallelFederation? = null

    // the theory and the flags the parallel solver holds, as text, if it was given any
    private var loaded: Pair<String, String>? = null

    // the link of the page joining the parallel solver, while enabled
    @Volatile private var joinUrl: String? = null

    // the address the others link to, while enabled
    @Volatile private var address: String? = null

    // the name given to the site, if any (see named)
    @Volatile private var name: String? = null

    /** Whether the federation is enabled: the parallel solver listening, linked to the parallel solvers paired */
    @Volatile var enabled: Boolean = false
        private set

    /** What the federation is now */
    val state: FederationState
        get() {
            val running = links?.takeIf { enabled }
            val stats = running?.stats
            return FederationState(
                enabled = enabled,
                name = running?.name ?: named(),
                code = running?.code,
                address = address.takeIf { running != null },
                joinUrl = joinUrl.takeIf { running != null },
                workers = threads,
                sites =
                    stats
                        ?.sites
                        .orEmpty()
                        .drop(1)
                        .map { SiteData(it.id, it.name.ifEmpty { null }, it.workers) },
            )
        }

    /**
     * Enables the federation, or disables it: the parallel solver made anew, its links closed (a query it solves is stopped);
     * an exception if it cannot listen, the federation disabled
     */
    fun enable(enabled: Boolean) {
        // already so: the query the parallel solver solves goes on
        if (enabled == this.enabled) return
        exclusively {
            // made so meanwhile, by another call
            if (enabled == this.enabled) return@exclusively
            this.enabled = enabled
            discard()
            try {
                if (enabled) parallel = made()
            } catch (failure: Exception) {
                this.enabled = false
                discard()
                throw failure
            } finally {
                notifyChanged()
            }
        }
    }

    /** Names the site [name], as the sites linking to it from now on are told (blank: the name of the host) */
    fun rename(name: String) {
        this.name = name.trim().ifEmpty { null }
        links?.name = named()
        notifyChanged()
    }

    /** A new code of the site, the one before refused from now on (the pairings made stay) */
    fun newCode() {
        links?.takeIf { enabled }?.newCode()
        notifyChanged()
    }

    /** Links the site to the one at [url], presenting [code] (none if they paired): an [Arg2pException] if it refuses it, or is not there */
    fun connect(
        url: String,
        code: String?,
    ) {
        val running = links?.takeIf { enabled } ?: throw Arg2pException("The federation is disabled")
        try {
            running.connect(url.trim(), code?.trim()?.ifEmpty { null })
        } catch (failure: Arg2pException) {
            throw failure
        } catch (failure: Exception) {
            throw Arg2pException(failure.message ?: "$url did not answer")
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

    /** Closes the links of the parallel solver, and stops it */
    override fun close() = exclusively { discard() }

    // [change] of the parallel solver, holding its lock: the query it solves, holding it meanwhile, stopped until it lets it go
    private fun exclusively(change: () -> Unit) {
        while (!lock.tryLock()) {
            parallel?.goals?.stop()
            if (lock.tryLock(STOPPING, TimeUnit.MILLISECONDS)) break
        }
        try {
            change()
        } finally {
            lock.unlock()
        }
    }

    /**
     * [work] on the parallel solver holding [theory] with [flags], one request at a time, what its solver prints going to
     * [output]: the theory and the flags given again if another request changed them since (the solutions given are
     * then computed again, and left out). Waited for on a thread of its own: an interruption of the thread asking stops
     * it, a CancellationException ([Resolution.STOPPED]).
     */
    fun <T> request(
        theory: String,
        flags: Flags,
        output: (String) -> Unit,
        work: (ThreadedSolver) -> T,
    ): T {
        lock.lockInterruptibly()
        try {
            val running = parallel ?: made().also { parallel = it }
            val asked =
                solving.submit {
                    val given = theory to flags.toString()
                    if (loaded != given) {
                        if (loaded?.second != given.second) running.flags = flags
                        running.load(theory)
                        loaded = given
                    }
                    running.goals.output = output
                    work(running)
                }
            return try {
                asked.get()
            } catch (failure: ExecutionException) {
                throw failure.cause ?: failure
            } catch (_: InterruptedException) {
                // stopped once asked: it ends at once, as stopped
                while (!asked.isDone) {
                    running.goals.stop()
                    runCatching { asked.get(STOPPING, TimeUnit.MILLISECONDS) }
                }
                throw CancellationException(Resolution.STOPPED)
            }
        } finally {
            lock.unlock()
        }
    }

    // A parallel solver, listening and heard if the federation is enabled, serving the page joining it
    private fun made(): ThreadedSolver {
        val made = ThreadedSolver(threads)
        try {
            if (enabled) links = federated(made)
            return made
        } catch (failure: Exception) {
            made.close()
            throw failure
        }
    }

    private fun federated(parallel: ThreadedSolver): ParallelFederation {
        val made = ParallelFederation(parallel)
        made.name = named()
        made.onPeersChanged = ::notifyChanged
        // the code required before the first link
        made.requiresCode = true
        made.serve(::page)
        val listened = made.listen(port)
        // (the page of a phone over TLS, with a certificate of its own: a browser gives its Web Workers no memory to share
        // otherwise, and the phone would lend nothing; the port listened on stays plain, for the sites nearby)
        address = lanAddress()?.let { "ws://$it:$listened" }
        joinUrl =
            lanAddress()?.let { address ->
                val ssl = SelfSigned.context(listOf(address, "localhost"))
                val secure =
                    runCatching { made.listenSecurely(ssl, listened + 1) }.getOrElse { made.listenSecurely(ssl) }
                "https://$address:$secure/"
            }
        return made
    }

    // The parallel solver closed, and forgotten
    private fun discard() {
        parallel?.close()
        parallel = null
        links = null
        address = null
        loaded = null
        joinUrl = null
    }

    private fun notifyChanged() = changed(state)

    // The name of the site: the one given, or else the name of the host, as the parallel solver names its site by default
    private fun named(): String = name ?: ParallelFederation.HOST_NAME

    companion object {
        /** Where the page joining a parallel solver loads arg2p from: the npm package of the version of the server */
        val MODULE = "https://cdn.jsdelivr.net/npm/@tuprolog/arg2p@${Arg2p.version()}/arg2p.mjs"

        // How long a request stopped is waited for before it is stopped again, in milliseconds
        private const val STOPPING = 200L

        // An address no packet is sent to (TEST-NET-1): the route to it is the one out of this host
        private const val OUTSIDE = "192.0.2.1"

        // The names of the interfaces of virtual machines and containers, which a phone does not reach
        private val VIRTUAL = Regex("virtual|vethernet|vmware|vbox|hyper-v|docker|wsl|tailscale|zerotier", RegexOption.IGNORE_CASE)

        // The page joining a parallel solver, and the script of its Web Workers, with the module of arg2p they load
        private val pages: Map<String, StaticContent> by lazy {
            mapOf(
                "/" to StaticContent(Pages.render("join.html", mapOf("module" to MODULE)).toByteArray(), "text/html; charset=utf-8"),
                "/worker.js" to
                    StaticContent(
                        "import { startWebWorker, nodeService } from '$MODULE';\nstartWebWorker(nodeService);\n"
                            .toByteArray(),
                        "text/javascript; charset=utf-8",
                    ),
            )
        }

        // The page served at [path] (its query left out): the page joining the parallel solver, or the script of its workers
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
 * The federation as the editor shows it: whether it is [enabled], the [name] of this site, its [code] and the [address]
 * the others link to (while enabled), the link of the page a phone joins it from ([joinUrl], none without an address on the local network), the
 * [workers] of the site and the [sites] it is federated with
 */
internal data class FederationState(
    val enabled: Boolean,
    val name: String,
    val code: String?,
    val address: String?,
    val joinUrl: String?,
    val workers: Int,
    val sites: List<SiteData>,
)

/** A site of the federation: its [name] (none for a browser), and its [workers] (none for no limit) */
internal data class SiteData(
    val id: String,
    val name: String?,
    val workers: Int?,
)
