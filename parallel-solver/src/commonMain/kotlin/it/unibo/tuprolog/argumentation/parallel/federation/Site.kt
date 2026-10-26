package it.unibo.tuprolog.argumentation.parallel.federation

import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.argumentation.parallel.message.Add
import it.unibo.tuprolog.argumentation.parallel.message.Address
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import it.unibo.tuprolog.argumentation.parallel.message.KbMessage
import it.unibo.tuprolog.argumentation.parallel.message.Load
import it.unibo.tuprolog.argumentation.parallel.message.MessageCodec
import it.unibo.tuprolog.argumentation.parallel.message.Rebuild
import it.unibo.tuprolog.argumentation.parallel.message.RequireEvaluation
import it.unibo.tuprolog.argumentation.parallel.message.Reset
import it.unibo.tuprolog.argumentation.parallel.node.Catalog
import it.unibo.tuprolog.argumentation.parallel.node.Network
import it.unibo.tuprolog.argumentation.parallel.node.Node
import it.unibo.tuprolog.argumentation.parallel.node.Worker
import kotlin.concurrent.Volatile
import kotlin.random.Random

/**
 * An instance of the distributed solver, on any platform: a catalog for a theory of its own, the workers it runs on
 * [scheduler] (at most [workers] for a graph, null for no limit), and its peers, other sites on the links its
 * transports give it (see [Link]), on this machine or another, whatever their platform.
 *
 * The graph of its theory is built by its workers and by those its peers lend (if they do, see [lends]): the theory
 * is split into as many parts as they can run together, or into [parts] if given, the parts placed on the sites in
 * proportion to their workers. The nodes of the site share the terms of their messages, written as text only towards
 * other sites (or always, with [textual]). A graph is built anew if a site running some of its workers leaves while
 * it is built; the workers it ran for a site that left are dropped.
 *
 * Each graph built is a session of its site: the workers of a session, wherever they run, only hear from those of the
 * same session, so that nothing of a graph given up reaches the next one.
 */
class Site(
    private val scheduler: Scheduler,
    private val workers: Int?,
    private val parts: Int? = null,
    settings: FlagsBuilder = FlagsBuilder(),
    private val lends: Boolean = true,
    private val textual: Boolean = false,
    val id: String = newId(),
) : LinkListener {
    private data class Peer(
        val link: Link,
        val capacity: Int?,
        val lends: Boolean,
    )

    private class Hosted(
        val queue: TaskQueue,
        val node: Node,
    )

    // A session known here: the sites of its workers by their addresses, and the sites linked to this one when the
    // session began, the only ones its messages go to directly (to the others through the site of the session), so
    // that the messages from a node to another always take the same way
    private class Session(
        val workers: Map<String, String>,
        val direct: Set<String>,
    )

    // The state shared by the threads of the site, as immutable values replaced under the lock: the peers by site, the
    // links of each, the sessions known and the workers run here (by session and address)
    private val lock = Any()

    @Volatile private var peers: Map<String, Peer> = emptyMap()

    @Volatile private var links: Map<Link, String> = emptyMap()

    @Volatile private var placements: Map<String, Session> = emptyMap()

    @Volatile private var hosted: Map<Pair<String, String>, Hosted> = emptyMap()

    // The session of the graph of the theory of this site, the one built last
    @Volatile private var session: String = "$id#0"

    private var sessions = 0
    private val clients = mutableMapOf<String, (EvaluationResponse) -> Unit>()
    private var clientIds = 0

    private val catalogQueue = scheduler.queue()

    // made at its first message: a site may only run workers for its peers
    private val catalog by lazy { Catalog(CatalogNetwork(), parts, settings) }

    /** The sites this one is federated with */
    val peerSites: Set<String> get() = peers.keys

    /** Called whenever a site joins or leaves the peers of this one */
    var onPeersChanged: (() -> Unit)? = null

    /** Replaces the theory with [theory], as Prolog text */
    fun load(theory: String) = toCatalog(Load(theory))

    /** Adds [clause], as Prolog text, to the theory */
    fun add(clause: String) = toCatalog(Add(clause))

    /** Forgets the theory */
    fun reset() = toCatalog(Reset)

    /** Evaluates [statement], as Prolog text: [reply] is given the arguments for it by label */
    fun solve(
        statement: String,
        reply: (EvaluationResponse) -> Unit,
    ) {
        val client = locked(lock) { "client_${clientIds++}".also { clients[it] = reply } }
        toCatalog(RequireEvaluation(statement, client))
    }

    override fun opened(link: Link) = link.send(FrameCodec.encode(Frame.Hello(id, VERSION, workers, lends)))

    override fun received(
        link: Link,
        frame: String,
    ) {
        when (val decoded = FrameCodec.decode(frame)) {
            is Frame.Hello -> greeted(link, decoded)
            is Frame.Placement -> locked(lock) { place(decoded.session, decoded.workers) }
            is Frame.Lost -> if (decoded.session == session) catalogQueue.post { catalog.receive(Rebuild) }
            is Frame.Deliver -> {
                val (session, to) = decoded
                val site = siteOf(session, to) ?: return
                when {
                    // a message between sites with no link between them, through the site of the session
                    site != id -> forward(session, site, frame)
                    to == Address.CATALOG ->
                        if (session == this.session) catalogQueue.post { catalog.receive(MessageCodec.decode(decoded.message)) }
                    else ->
                        worker(session, to)?.let { worker ->
                            worker.queue.post { worker.node.receive(MessageCodec.decode(decoded.message)) }
                        }
                }
            }
        }
    }

    override fun closed(link: Link) {
        val broken =
            locked(lock) {
                val site = links[link]
                links = links - link
                val replacement = links.entries.firstOrNull { it.value == site }?.key
                when {
                    site == null || peers[site]?.link != link -> emptyList()
                    replacement != null -> {
                        peers = peers + (site to peers.getValue(site).copy(link = replacement))
                        emptyList()
                    }
                    else -> {
                        peers = peers - site
                        // the workers it ran for its graphs are of no use any more
                        placements = placements.filterKeys { ownerOf(it) != site }
                        hosted = hosted.filterKeys { ownerOf(it.first) != site }
                        // the sessions whose messages went to it directly: some may be lost
                        placements.filterValues { site in it.direct }.keys.toList()
                    }
                }
            }
        broken.forEach { session ->
            if (ownerOf(session) == id) {
                if (session == this.session) catalogQueue.post { catalog.receive(Rebuild) }
            } else {
                peers[ownerOf(session)]?.link?.send(FrameCodec.encode(Frame.Lost(session)))
            }
        }
        onPeersChanged?.invoke()
    }

    // A peer known from its first frame, unless it is this very site or of another version: a second link to the same
    // site is kept in case the first closes
    private fun greeted(
        link: Link,
        hello: Frame.Hello,
    ) {
        if (hello.site == id || hello.version != VERSION) return link.close()
        locked(lock) {
            links = links + (link to hello.site)
            if (hello.site !in peers) peers = peers + (hello.site to Peer(link, hello.capacity, hello.lends))
        }
        onPeersChanged?.invoke()
    }

    // The placement of the workers of [session]: those of the earlier sessions of the same site are dropped
    private fun place(
        session: String,
        workers: Map<String, String>,
    ) {
        val owner = ownerOf(session)
        val direct = (workers.values.toSet() + owner).filter { it in peers }.toSet()
        placements = placements.filterKeys { ownerOf(it) != owner } + (session to Session(workers, direct))
        hosted = hosted.filterKeys { ownerOf(it.first) != owner }
    }

    // The site of the node at [to] in [session], if known
    private fun siteOf(
        session: String,
        to: String,
    ): String? = if (to == Address.CATALOG) ownerOf(session) else placements[session]?.workers?.get(to)

    // [frame] of [session] sent towards [site]: directly if the session goes there so, through its site otherwise
    private fun forward(
        session: String,
        site: String,
        frame: String,
    ) {
        val direct = placements[session]?.direct?.contains(site) ?: true
        peers[if (direct) site else ownerOf(session)]?.link?.send(frame)
    }

    private fun toCatalog(message: KbMessage) = deliver(catalogQueue, catalog, message)

    private fun deliver(
        queue: TaskQueue,
        node: Node,
        message: KbMessage,
    ) {
        if (textual) {
            val text = MessageCodec.encode(message)
            queue.post { node.receive(MessageCodec.decode(text)) }
        } else {
            queue.post { node.receive(message) }
        }
    }

    // The worker at [address] for [session], run here: made at its first message, if the session is known
    private fun worker(
        session: String,
        address: String,
    ): Hosted? =
        hosted[session to address] ?: locked(lock) {
            if (session !in placements) {
                null
            } else {
                hosted[session to address] ?: Hosted(scheduler.queue(), Worker(address, WorkerNetwork(session))).also {
                    hosted = hosted + ((session to address) to it)
                }
            }
        }

    // A message of [session] to the node at [to]: to a client of this site, to a node here, or to the site of the node
    private fun route(
        session: String,
        to: String,
        message: KbMessage,
    ) {
        val site =
            when {
                to == Address.CATALOG || Address.isWorker(to) -> siteOf(session, to) ?: return
                else -> return locked(lock) { clients.remove(to) }?.invoke(message as EvaluationResponse) ?: Unit
            }
        when {
            site != id -> forward(session, site, FrameCodec.encode(Frame.Deliver(session, to, MessageCodec.encode(message))))
            to == Address.CATALOG -> if (session == this.session) toCatalog(message)
            else -> worker(session, to)?.let { deliver(it.queue, it.node, message) }
        }
    }

    // The workers of a graph, in proportion to the workers of the sites (this one and the peers lending theirs), each
    // site given consecutive parts
    private fun placement(
        count: Int,
        lenders: Map<String, Peer>,
    ): Map<String, String> {
        val sites = listOf(id to workers) + lenders.map { (site, peer) -> site to peer.capacity }
        val weights = sites.map { (_, capacity) -> (capacity ?: count).toDouble() }
        val total = weights.sum()
        if (total == 0.0) return (0 until count).associate { Address.worker(it) to id }
        val quotas = weights.map { it * count / total }
        val shares = quotas.map { it.toInt() }.toMutableList()
        quotas.indices
            .sortedByDescending { quotas[it] - shares[it] }
            .take(count - shares.sum())
            .forEach { shares[it]++ }
        return sites.indices
            .flatMap { site -> List(shares[site]) { sites[site].first } }
            .withIndex()
            .associate { (index, site) -> Address.worker(index) to site }
    }

    private inner class CatalogNetwork : Network {
        override fun send(
            to: String,
            message: KbMessage,
        ) = route(session, to, message)

        // The workers of this site and those of its peers lending theirs, no limit if one has none: one at least, run
        // here if no site has any
        override val capacity: Int?
            get() {
                val lent = peers.values.filter { it.lends }.map { it.capacity }
                return if (workers == null || null in lent) null else maxOf(1, workers + lent.sumOf { it ?: 0 })
            }

        override fun building(workers: Int) {
            val (lenders, frame) =
                locked(lock) {
                    val next = "$id#${++sessions}"
                    val lenders = peers.filterValues { it.lends }
                    val placed = placement(workers, lenders)
                    place(next, placed)
                    session = next
                    lenders.values to FrameCodec.encode(Frame.Placement(next, placed))
                }
            // every lender is told, so that those placed none drop the workers of the graphs built before
            lenders.forEach { it.link.send(frame) }
        }
    }

    private inner class WorkerNetwork(
        private val session: String,
    ) : Network {
        override fun send(
            to: String,
            message: KbMessage,
        ) = route(session, to, message)
    }

    companion object {
        /** The version a site tells its peers: sites of different versions do not federate */
        const val VERSION: String = "arg2p-parallel/1 $ARG2P_VERSION"

        // A site identifier with no '#', which separates it from the number of a session
        private fun newId() = "site-" + Random.nextLong().toULong().toString(16)

        private fun ownerOf(session: String) = session.substringBeforeLast('#')
    }
}
