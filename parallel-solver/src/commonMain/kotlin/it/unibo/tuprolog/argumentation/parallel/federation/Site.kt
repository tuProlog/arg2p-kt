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
 * it is built, or a link its messages went through closes; the workers it ran for a site that left are dropped.
 *
 * A site may have delegates, sites working for it alone (the threads of a JS runtime, see [delegate]): it offers
 * their workers with its own, and runs the workers it is given on them as well, its delegates reaching the other
 * sites through it.
 *
 * Sites opening links of their own ([introducible], as browsers over WebRTC) are introduced to each other by the sites
 * they are both linked to, which carry what they need to open the link ([signal]).
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
    private val introducible: Boolean = false,
    val id: String = newId(),
) : LinkListener {
    private data class Peer(
        val link: Link,
        val capacity: Int?,
        val lends: Boolean,
        val delegate: Boolean,
        val introducible: Boolean,
    )

    private class Hosted(
        val queue: TaskQueue,
        val node: Node,
    )

    // A session known here: the sites of its workers by their addresses, the sites linked to this one when the
    // session began, the only ones its messages go to directly, and the site through which they reach the others (the
    // one that placed the workers here), so that the messages from a node to another always take the same way
    private class Session(
        val workers: Map<String, String>,
        val direct: Set<String>,
        val via: String,
    )

    // The state shared by the threads of the site, as immutable values replaced under the lock: the peers by site, the
    // links of each, the links to the delegates, the sessions known and the workers run here (by session and address)
    private val lock = Any()

    @Volatile private var peers: Map<String, Peer> = emptyMap()

    @Volatile private var links: Map<Link, String> = emptyMap()

    @Volatile private var delegateLinks: Set<Link> = emptySet()

    @Volatile private var placements: Map<String, Session> = emptyMap()

    @Volatile private var hosted: Map<Pair<String, String>, Hosted> = emptyMap()

    // The sites this one was introduced to, by the site that introduced them
    @Volatile private var introducers: Map<String, String> = emptyMap()

    // The session of the graph of the theory of this site, the one built last
    @Volatile private var session: String = "$id#0"

    private var sessions = 0
    private val clients = mutableMapOf<String, (EvaluationResponse) -> Unit>()
    private var clientIds = 0

    private val catalogQueue = scheduler.queue()

    // made at its first message: a site may only run workers for its peers
    private val catalog by lazy { Catalog(CatalogNetwork(), parts, settings) }

    /** The sites this one is federated with, its delegates included */
    val peerSites: Set<String> get() = peers.keys

    // The workers this site runs, for the tests
    internal val workersRun: Int get() = hosted.size

    // The arguments of the graph of the theory of this site, for the tests
    internal val argumentsBuilt: Int get() = catalog.argumentsBuilt

    /** Called whenever a site joins or leaves the peers of this one */
    var onPeersChanged: (() -> Unit)? = null

    /** Called with a site opening links of its own this one is introduced to, if it opens such links as well */
    var onIntroduced: ((site: String) -> Unit)? = null

    /** Called with what a site sent this one to open a link of their own (see [signal]) */
    var onSignal: ((from: String, payload: String) -> Unit)? = null

    /** Sends [payload] to [to], a peer or a site this one was introduced to, to open a link of their own */
    fun signal(
        to: String,
        payload: String,
    ) {
        val next = if (to in peers) to else introducers[to]
        next?.let { peers[it]?.link?.send(FrameCodec.encode(Frame.Signal(id, to, payload))) }
    }

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

    /** [link], just opened, leads to a delegate of this site: the site offers its workers, and runs workers on them */
    fun delegate(link: Link) {
        locked(lock) { delegateLinks = delegateLinks + link }
        opened(link)
    }

    override fun opened(link: Link) = link.send(hello(link))

    override fun received(
        link: Link,
        frame: String,
    ) {
        when (val decoded = FrameCodec.decode(frame)) {
            is Frame.Hello -> greeted(link, decoded)
            is Frame.Placement -> placed(link, decoded)
            is Frame.Forget -> locked(lock) { forget(decoded.site) }
            is Frame.Introduce ->
                if (introducible && decoded.site != id && decoded.site !in peers) {
                    links[link]?.let { introducer -> locked(lock) { introducers = introducers + (decoded.site to introducer) } }
                    onIntroduced?.invoke(decoded.site)
                }
            is Frame.Signal ->
                if (decoded.to == id) {
                    onSignal?.invoke(decoded.from, decoded.payload)
                } else {
                    peers[decoded.to]?.link?.send(frame)
                }
            is Frame.Lost ->
                if (ownerOf(decoded.session) != id) {
                    forward(decoded.session, ownerOf(decoded.session), frame)
                } else if (decoded.session == session) {
                    catalogQueue.post { catalog.receive(Rebuild) }
                }
            is Frame.Deliver -> {
                val (session, to) = decoded
                val site = siteOf(session, to) ?: return
                when {
                    // a message between sites with no link between them, or to a delegate
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
        val (lost, broken) =
            locked(lock) {
                val site = links[link]
                links = links - link
                delegateLinks = delegateLinks - link
                val replacement = links.entries.firstOrNull { it.value == site }?.key
                when {
                    site == null || peers[site]?.link != link -> null to emptyList()
                    replacement != null -> {
                        peers = peers + (site to peers.getValue(site).copy(link = replacement))
                        null to emptyList()
                    }
                    else -> {
                        val peer = peers.getValue(site)
                        peers = peers - site
                        forget(site)
                        // the sessions whose messages went to it directly: some may be lost
                        (site to peer.delegate) to placements.filterValues { site in it.direct }.keys.toList()
                    }
                }
            }
        val (site, delegate) = lost ?: return
        broken.forEach { session ->
            if (ownerOf(session) == id) {
                if (session == this.session) catalogQueue.post { catalog.receive(Rebuild) }
            } else {
                forward(session, ownerOf(session), FrameCodec.encode(Frame.Lost(session)))
            }
        }
        // the delegates forget the workers they ran for it, the other sites learn of the workers offered now
        if (delegate) announce() else toDelegates(FrameCodec.encode(Frame.Forget(site)))
        onPeersChanged?.invoke()
    }

    // The first frame on [link]: a peer known, unless it is this very site or of another version, or the workers it
    // offers updated. A second link to the same site is kept in case the first closes.
    private fun greeted(
        link: Link,
        hello: Frame.Hello,
    ) {
        if (hello.site == id || hello.version != VERSION) return link.close()
        val joined =
            locked(lock) {
                links = links + (link to hello.site)
                val known = peers[hello.site]
                when {
                    known == null -> {
                        val delegate = link in delegateLinks
                        peers = peers + (hello.site to Peer(link, hello.capacity, hello.lends, delegate, hello.introducible))
                        true
                    }
                    known.link == link -> {
                        peers = peers + (hello.site to known.copy(capacity = hello.capacity, lends = hello.lends))
                        false
                    }
                    else -> false
                }
            }
        if (joined) {
            if (peers[hello.site]?.delegate == true) announce()
            if (hello.introducible) introduce(hello.site)
            onPeersChanged?.invoke()
        }
    }

    // The workers placed here for [placement], run here and on the delegates in proportion to their workers: the
    // delegates are told where they all are
    private fun placed(
        link: Link,
        placement: Frame.Placement,
    ) {
        val delegated =
            locked(lock) {
                val delegates = peers.filterValues { it.delegate }
                val mine =
                    placement.workers
                        .filterValues { it == id }
                        .keys
                        .sortedBy { Address.indexOf(it) }
                val workers =
                    if (delegates.isEmpty() || mine.isEmpty()) {
                        placement.workers
                    } else {
                        placement.workers + share(mine, delegates)
                    }
                place(placement.session, workers, links[link] ?: ownerOf(placement.session))
                workers
            }
        toDelegates(FrameCodec.encode(Frame.Placement(placement.session, delegated)))
    }

    // The placement of the workers of [session], reaching the sites with no link to this one through [via]: those of
    // the earlier sessions of the same site are dropped
    private fun place(
        session: String,
        workers: Map<String, String>,
        via: String,
    ) {
        forget(ownerOf(session))
        val direct = (workers.values.toSet() + ownerOf(session)).filter { it in peers }.toSet()
        placements = placements + (session to Session(workers, direct, via))
    }

    // The sessions of [site] and their workers forgotten
    private fun forget(site: String) {
        placements = placements.filterKeys { ownerOf(it) != site }
        hosted = hosted.filterKeys { ownerOf(it.first) != site }
    }

    // The site of the node at [to] in [session], if known
    private fun siteOf(
        session: String,
        to: String,
    ): String? = if (to == Address.CATALOG) ownerOf(session) else placements[session]?.workers?.get(to)

    // [frame] of [session] sent towards [site]: directly if the session goes there so, through the site it came
    // through otherwise
    private fun forward(
        session: String,
        site: String,
        frame: String,
    ) {
        val known = placements[session]
        val next = if (known == null || site in known.direct) site else known.via
        peers[next]?.link?.send(frame)
    }

    private fun toDelegates(frame: String) = peers.values.filter { it.delegate }.forEach { it.link.send(frame) }

    // The first frame on [link]: to a delegate, the workers of this site alone; to another site, those of its
    // delegates as well (null if one has no limit)
    private fun hello(link: Link): String {
        val offered =
            if (link in delegateLinks) {
                workers
            } else {
                val delegated = peers.values.filter { it.delegate }.map { it.capacity }
                if (workers == null || null in delegated) null else workers + delegated.sumOf { it ?: 0 }
            }
        return FrameCodec.encode(Frame.Hello(id, VERSION, offered, lends, introducible))
    }

    // [site], just joined, and the other peers opening links of their own introduced to each other
    private fun introduce(site: String) {
        val joined = peers[site] ?: return
        peers
            .filter { (other, peer) -> other != site && peer.introducible && !peer.delegate }
            .forEach { (other, peer) ->
                peer.link.send(FrameCodec.encode(Frame.Introduce(site)))
                joined.link.send(FrameCodec.encode(Frame.Introduce(other)))
            }
    }

    // The other sites told again of the workers this one offers, as its delegates come and go
    private fun announce() = peers.values.filterNot { it.delegate }.forEach { it.link.send(hello(it.link)) }

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

    // The workers at [addresses] shared among this site and [others], in proportion to their workers, each site given
    // consecutive ones
    private fun share(
        addresses: List<String>,
        others: Map<String, Peer>,
    ): Map<String, String> {
        val sites = listOf(id to workers) + others.map { (site, peer) -> site to peer.capacity }
        val weights = sites.map { (_, capacity) -> (capacity ?: addresses.size).toDouble() }
        val total = weights.sum()
        if (total == 0.0) return addresses.associateWith { id }
        val quotas = weights.map { it * addresses.size / total }
        val shares = quotas.map { it.toInt() }.toMutableList()
        quotas.indices
            .sortedByDescending { quotas[it] - shares[it] }
            .take(addresses.size - shares.sum())
            .forEach { shares[it]++ }
        return sites.indices
            .flatMap { site -> List(shares[site]) { sites[site].first } }
            .zip(addresses) { site, address -> address to site }
            .toMap()
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
                    val placed = share(List(workers) { Address.worker(it) }, lenders)
                    place(next, placed, id)
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
