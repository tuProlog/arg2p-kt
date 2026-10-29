package it.unibo.tuprolog.argumentation.parallel.federation

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.message.Add
import it.unibo.tuprolog.argumentation.parallel.message.Address
import it.unibo.tuprolog.argumentation.parallel.message.AnswerResponse
import it.unibo.tuprolog.argumentation.parallel.message.Configure
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import it.unibo.tuprolog.argumentation.parallel.message.KbMessage
import it.unibo.tuprolog.argumentation.parallel.message.Load
import it.unibo.tuprolog.argumentation.parallel.message.MessageCodec
import it.unibo.tuprolog.argumentation.parallel.message.NO_GRAPH
import it.unibo.tuprolog.argumentation.parallel.message.Rebuild
import it.unibo.tuprolog.argumentation.parallel.message.RequireAnswer
import it.unibo.tuprolog.argumentation.parallel.message.RequireEvaluation
import it.unibo.tuprolog.argumentation.parallel.message.RequireSolution
import it.unibo.tuprolog.argumentation.parallel.message.Reset
import it.unibo.tuprolog.argumentation.parallel.message.STOPPED
import it.unibo.tuprolog.argumentation.parallel.message.SolutionResponse
import it.unibo.tuprolog.argumentation.parallel.message.Stop
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
 * A site may require a [code] of the sites linking to it ([requiresCode]): one proving it knows it is paired with it,
 * and proves it knows the token of their pairing instead from then on, linked again with no code while the two run;
 * one proving no such secret is refused, and after a few wrong codes from an address its codes are refused for a
 * while. Neither secret goes over a link (see [Pairing]), and a site linking to another with one checks that the
 * other knows it as well. Nothing but the greetings is read on a link before its site is admitted.
 *
 * Each graph built is a session of its site: the workers of a session, wherever they run, only hear from those of the
 * same session, so that nothing of a graph given up reaches the next one.
 *
 * The catalog of the site evaluates the statements of its clients ([solve]) and solves their goals as the standard
 * solver does (see [it.unibo.tuprolog.argumentation.parallel.cluster.ClusterSolver]); [stats] tells what the
 * federation does, as the site knows it.
 */
class Site(
    private val scheduler: Scheduler,
    private val workers: Int?,
    private val parts: Int? = null,
    flags: Flags = Flags.defaults(),
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

    // The last evaluation the catalog answered
    @Volatile private var evaluation: EvaluationStats? = null

    // The code of the site and its pairings, the links this site opened waiting to be greeted (told then, or told why
    // they are refused), and the sites heard on the local network
    private val pairing = Pairing()
    private val greetings = mutableMapOf<Link, (refusal: String?) -> Unit>()

    // The links open, each with the nonce this site gave on it (for the other to prove its secret on) and whether it
    // greeted the other site on it
    private class Opening(
        val nonce: String,
    ) {
        var greeted = false
    }

    private val openings = mutableMapOf<Link, Opening>()

    @Volatile private var heard: List<NearbySite> = emptyList()

    private var sessions = 0

    // The clients waiting for an answer: how each is given it, and what it is given if stopped
    private val clients = mutableMapOf<String, Pair<(KbMessage) -> Unit, KbMessage>>()
    private var clientIds = 0
    private var runIds = 0

    private val catalogQueue = scheduler.queue()

    // made at its first message: a site may only run workers for its peers
    private val catalog by lazy { Catalog(CatalogNetwork(), parts, flags) }

    /** The sites this one is federated with, its delegates included */
    val peerSites: Set<String> get() = peers.keys

    /** The name of the site, as its beacons tell the sites nearby (see [Beacons]) */
    @Volatile var name: String = ""

    /**
     * Whether the sites linking to this one must present its [code], or the token of a pairing made with it: refused
     * otherwise. Its delegates and the links it opens are not checked.
     */
    @Volatile var requiresCode: Boolean = false

    /**
     * The code the sites linking to this one prove they know, if it requires one: four digits, random, renewed once a
     * site pairs with this one
     */
    val code: String get() = pairing.code

    /** A new [code], the one before refused from now on (the pairings made stay) */
    fun newCode(): String = pairing.renew()

    /** The sites paired with this one (one proved the code of the other): linked again with no code while they run */
    val pairedSites: Set<String> get() = pairing.paired

    /** The sites heard on the local network (see [Beacons]) that are not peers of this one */
    val nearby: List<NearbySite> get() = heard.filter { it.id !in peers }

    /** Called whenever the sites heard on the local network change */
    var onNearbyChanged: (() -> Unit)? = null

    // The sites heard on the local network, now
    internal fun heard(sites: List<NearbySite>) {
        if (sites == heard) return
        heard = sites
        onNearbyChanged?.invoke()
    }

    // The token of the pairing with [site], to present linking to it again
    internal fun tokenOf(site: String): String? = pairing.tokenOf(site)

    // [link], opened by this site, told [done] once the site at its other end greets this one (null), refuses it or
    // closes (why)
    internal fun greeting(
        link: Link,
        done: (refusal: String?) -> Unit,
    ) = locked(lock) { greetings[link] = done }

    // [link] no longer waited for (see [greeting]): its waiter gave up
    internal fun forgetGreeting(link: Link) = locked(lock) { greetings.remove(link) }

    // Whether the site at the other end of [link] greeted this one, and was admitted
    internal fun admitted(link: Link): Boolean = link in links

    // The workers this site runs, for the tests
    internal val workersRun: Int get() = hosted.size

    // The arguments of the graph of the theory of this site, for the tests
    internal val argumentsBuilt: Int get() = catalog.argumentsBuilt

    internal val graphArguments: List<it.unibo.tuprolog.core.Term> get() = catalog.graphArguments

    /**
     * What the federation does, as this site knows it: itself, its peers, the sites it reaches through them (those
     * introduced to it, and those running workers of a graph it runs some of), and the last evaluation it answered
     */
    val stats: FederationStats
        get() {
            val linked = peers
            val reached =
                introducers.toList() +
                    placements.flatMap { (session, known) -> (known.workers.values + ownerOf(session)).map { it to known.via } }
            val through = reached.filter { (site, via) -> site != id && site !in linked && via != id }.toMap()
            val sites =
                listOf(SiteStats(id, offered, delegate = false, via = null)) +
                    linked.map { (site, peer) -> SiteStats(site, peer.capacity, peer.delegate, via = null) } +
                    through.map { (site, via) -> SiteStats(site, workers = null, delegate = false, via = via) }
            return FederationStats(id, sites, evaluation, nearby)
        }

    /** Called whenever a site joins or leaves the peers of this one */
    var onPeersChanged: (() -> Unit)? = null

    // Called with a site opening links of its own this one is introduced to, if it opens such links as well: by the
    // transport opening those links (see RtcLinks)
    internal var onIntroduced: ((site: String) -> Unit)? = null

    // Called with what a site sent this one to open a link of their own (see [signal]), by the same transport
    internal var onSignal: ((from: String, payload: String) -> Unit)? = null

    /** Sends [payload] to [to], a peer or a site this one was introduced to, to open a link of their own */
    fun signal(
        to: String,
        payload: String,
    ) {
        val next = if (to in peers) to else introducers[to]
        next?.let { peers[it]?.link?.send(FrameCodec.encode(Frame.Signal(id, to, payload))) }
    }

    /** The flags of the solver of the catalog: those set apply from the next evaluation on, the graph built anew */
    var flags: Flags = flags
        set(value) {
            field = value
            toCatalog(Configure(value.toString()))
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
    ) = ask({ RequireEvaluation(statement, it) }, EvaluationResponse(emptyList(), emptyList(), emptyList(), STOPPED)) {
        reply(it as EvaluationResponse)
    }

    // Solves [goal], Prolog text, as the standard solver does, in a run of its own: [reply] is given its first solution
    internal fun query(
        goal: String,
        reply: (SolutionResponse) -> Unit,
    ) {
        val run = locked(lock) { "run_${runIds++}" }
        ask({ RequireSolution(run, goal, it) }, stopped(run)) { reply(it as SolutionResponse) }
    }

    // The next solution of the goal solved in [run]
    internal fun next(
        run: String,
        reply: (SolutionResponse) -> Unit,
    ) = ask({ RequireSolution(run, null, it) }, stopped(run)) { reply(it as SolutionResponse) }

    // The statements for [query], Prolog text, by label, as the standard solver answers it
    internal fun answer(
        query: String,
        reply: (AnswerResponse) -> Unit,
    ) = ask({ RequireAnswer(query, it) }, AnswerResponse(null, STOPPED)) { reply(it as AnswerResponse) }

    /**
     * Stops what the catalog evaluates for the clients of this site: their evaluations end at once, as stopped, and
     * the graph being built for them is given up (the resolution under way interrupted, where it can be); the next
     * ones are answered as before. Nothing is done if none is under way.
     */
    fun stop() {
        val stopped = locked(lock) { clients.values.toList().also { clients.clear() } }
        if (stopped.isEmpty()) return
        catalogQueue.interrupt()
        toCatalog(Stop)
        stopped.forEach { (reply, answer) -> reply(answer) }
    }

    // The catalog asked [request], for a client of this site given its answer ([stopped] if stopped)
    private fun ask(
        request: (client: String) -> KbMessage,
        stopped: KbMessage,
        reply: (KbMessage) -> Unit,
    ) {
        val client = locked(lock) { "client_${clientIds++}".also { clients[it] = reply to stopped } }
        toCatalog(request(client))
    }

    // What a client solving a goal in [run] is given if stopped
    private fun stopped(run: String) = SolutionResponse(run, null, NO_GRAPH, false, "", stopped = true)

    /** [link], just opened, leads to a delegate of this site: the site offers its workers, and runs workers on them */
    fun delegate(link: Link) {
        locked(lock) { delegateLinks = delegateLinks + link }
        opened(link)
    }

    // Every link begins with the greetings of its ends: on a link to a delegate, this site greets the other at once; on
    // a link it opened, once the other spoke first (challenging it, or greeting it); on another link, it challenges the
    // other first, with a nonce to prove its secret on (see [Pairing]), and greets it once greeted
    override fun opened(link: Link) {
        val nonce = locked(lock) { openingOf(link).nonce }
        when {
            link in delegateLinks -> greet(link)
            link.outgoing || link in links -> Unit
            else -> link.send(FrameCodec.encode(Frame.Challenge(nonce)))
        }
    }

    // Whether a site linking to this one on [link] must prove it knows its code
    private fun guarded(link: Link) = requiresCode && !link.outgoing && link !in delegateLinks

    // The opening of [link], made at its first frame if that comes before the link is told open (as on a link in memory)
    private fun openingOf(link: Link): Opening = openings.getOrPut(link) { Opening(Pairing.newNonce()) }

    // A frame that is none (a malformed one, or one of another version) closes its link alone, as one coming before the
    // greetings of its site
    override fun received(
        link: Link,
        frame: String,
    ) {
        val decoded = runCatching { FrameCodec.decode(frame) }.getOrElse { return link.close() }
        if (link !in links) return greeting(link, decoded)
        when (decoded) {
            is Frame.Hello -> greetedAgain(link, decoded)
            is Frame.Challenge, is Frame.Refused -> Unit
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
                    toCatalogOf(decoded.session, Rebuild)
                }
            is Frame.Deliver -> {
                val (session, to) = decoded
                val site = siteOf(session, to) ?: return
                when {
                    // a message between sites with no link between them, or to a delegate
                    site != id -> forward(session, site, frame)
                    to == Address.CATALOG -> message(link, decoded.message)?.let { toCatalogOf(session, it) }
                    else ->
                        worker(session, to)?.let { worker ->
                            // read on the thread of the worker, as reading its terms takes a while
                            worker.queue.post { message(link, decoded.message)?.let { worker.node.receive(it) } }
                        }
                }
            }
        }
    }

    // The message written in [text], from the other end of [link]: none if it is not one, the link closed then
    private fun message(
        link: Link,
        text: String,
    ): KbMessage? = runCatching { MessageCodec.decode(text) }.getOrElse { null.also { link.close() } }

    // A frame on [link] before its site is admitted: its greetings alone, any other closing it
    private fun greeting(
        link: Link,
        frame: Frame,
    ) = when (frame) {
        is Frame.Challenge -> greet(link, proof = link.code?.let { Pairing.proof(it, id, frame.nonce) })
        is Frame.Hello -> greeted(link, frame)
        is Frame.Refused -> refused(link, frame.reason)
        else -> link.close()
    }

    // This site greeting the other on [link], once: with the proof of its secret on the nonce of the other, and the
    // token of the pairing made just now with it
    private fun greet(
        link: Link,
        proof: String? = null,
        token: String? = null,
    ) {
        val nonce = locked(lock) { openingOf(link).takeUnless { it.greeted }?.also { it.greeted = true }?.nonce } ?: return
        link.send(hello(link, nonce, proof, token))
    }

    // [link], opened by this site, refused (why told to its waiter, see [greeting]), and closed
    private fun refused(
        link: Link,
        reason: String,
    ) {
        locked(lock) { greetings.remove(link) }?.invoke(reason)
        link.close()
    }

    override fun closed(link: Link) {
        locked(lock) { greetings.remove(link) }?.invoke("The link closed")
        val (lost, broken) =
            locked(lock) {
                val site = links[link]
                links = links - link
                openings -= link
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
                toCatalogOf(session, Rebuild)
            } else {
                forward(session, ownerOf(session), FrameCodec.encode(Frame.Lost(session)))
            }
        }
        // the delegates forget the workers they ran for it, the other sites learn of the workers offered now
        if (delegate) announce() else toDelegates(FrameCodec.encode(Frame.Forget(site)))
        onPeersChanged?.invoke()
    }

    // The greeting of the site at the other end of [link], not admitted yet: refused if it is this very site or of
    // another version. On a link this site opened presenting a secret (a code, or the token of a pairing), the other
    // site proves it knows it as well, its token remembered if it gave one, or the link is closed (a site claiming the
    // identifier of another, say); on another link, a site that did not prove it knows the code this one requires, or
    // the token of their pairing, is refused (and told). The site admitted is greeted in turn, with the proof of the
    // secret it proved, on its nonce: a peer then, or a second link to a peer, kept in case the first closes.
    private fun greeted(
        link: Link,
        hello: Frame.Hello,
    ) {
        if (hello.site == id || hello.version != VERSION) return link.close()
        val nonce = locked(lock) { openingOf(link).nonce }
        val secret = link.code.takeIf { link.outgoing }
        val proven =
            when {
                secret != null -> {
                    if (!Pairing.proves(
                            hello.proof,
                            secret,
                            hello.site,
                            nonce,
                        )
                    ) {
                        return refused(link, "${hello.site} did not prove the code")
                    }
                    hello.token?.let { pairing.remember(hello.site, it) }
                    null
                }
                link.outgoing -> null
                guarded(link) ->
                    when (val admission = pairing.admission(hello.site, link.address, nonce, hello.proof)) {
                        is Pairing.Refusal -> {
                            link.send(FrameCodec.encode(Frame.Refused(admission.reason)))
                            return link.close()
                        }
                        is Pairing.Proven -> admission
                    }
                else -> hello.proof?.let { pairing.proven(hello.site, nonce, it) }
            }
        val joined =
            locked(lock) {
                links = links + (link to hello.site)
                (hello.site !in peers).also { joining ->
                    val delegate = link in delegateLinks
                    if (joining) peers = peers + (hello.site to Peer(link, hello.capacity, hello.lends, delegate, hello.introducible))
                }
            }
        greet(link, proven?.let { known -> hello.nonce?.let { Pairing.proof(known.secret, id, it) } }, proven?.token)
        locked(lock) { greetings.remove(link) }?.invoke(null)
        if (joined) {
            if (peers[hello.site]?.delegate == true) announce()
            if (hello.introducible) introduce(hello.site)
            onPeersChanged?.invoke()
        }
    }

    // The greeting of a peer again on [link]: the workers it offers updated (see [announce]); a greeting of another
    // site closes the link, which stays that of the site first greeted on it
    private fun greetedAgain(
        link: Link,
        hello: Frame.Hello,
    ) {
        val same =
            locked(lock) {
                (links[link] == hello.site).also { same ->
                    val known = peers[hello.site]
                    if (same && known?.link == link) {
                        peers = peers + (hello.site to known.copy(capacity = hello.capacity, lends = hello.lends))
                    }
                }
            }
        if (!same) link.close()
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

    // The greeting of this site on [link] (see [Frame.Hello]): to a delegate, the workers of this site alone; to another
    // site, those of its delegates as well (null if one has no limit)
    private fun hello(
        link: Link,
        nonce: String? = null,
        proof: String? = null,
        token: String? = null,
    ): String {
        val capacity = if (link in delegateLinks) workers else offered
        return FrameCodec.encode(Frame.Hello(id, VERSION, capacity, lends, introducible, nonce, proof, token))
    }

    // The workers this site offers the other sites: its own and those of its delegates (null if one has no limit)
    private val offered: Int?
        get() {
            val delegated = peers.values.filter { it.delegate }.map { it.capacity }
            return if (workers == null || null in delegated) null else workers + delegated.sumOf { it ?: 0 }
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

    // [message] of the graph of [session] to the catalog: handled if the catalog still builds that graph when it comes
    // to it (one it gave up builds a graph anew, and the messages of the old one queued before reach none)
    private fun toCatalogOf(
        session: String,
        message: KbMessage,
    ) = deliver(catalogQueue, catalog, message) { session == this.session }

    private fun deliver(
        queue: TaskQueue,
        node: Node,
        message: KbMessage,
        current: () -> Boolean = { true },
    ) {
        if (textual) {
            val text = MessageCodec.encode(message)
            queue.post { if (current()) node.receive(MessageCodec.decode(text)) }
        } else {
            queue.post { if (current()) node.receive(message) }
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
                else -> return locked(lock) { clients.remove(to) }?.first?.invoke(message) ?: Unit
            }
        when {
            site != id -> forward(session, site, FrameCodec.encode(Frame.Deliver(session, to, MessageCodec.encode(message))))
            to == Address.CATALOG -> toCatalogOf(session, message)
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

        override fun siteOf(address: String): String = siteOf(session, address) ?: id

        override fun evaluated(stats: EvaluationStats) {
            evaluation = stats
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
        const val VERSION: String = "arg2p-parallel/2 $ARG2P_VERSION"

        // A site identifier with no '#', which separates it from the number of a session
        private fun newId() = "site-" + Random.nextLong().toULong().toString(16)

        private fun ownerOf(session: String) = session.substringBeforeLast('#')
    }
}
