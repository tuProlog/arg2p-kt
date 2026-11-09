package it.unibo.tuprolog.argumentation.federation

import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi
import it.unibo.tuprolog.argumentation.parallel.ParallelNode
import it.unibo.tuprolog.argumentation.parallel.host.ConsumerHost
import it.unibo.tuprolog.argumentation.parallel.host.HostCodec
import it.unibo.tuprolog.argumentation.parallel.host.HostFrame
import it.unibo.tuprolog.argumentation.parallel.host.locked
import kotlin.concurrent.Volatile

/**
 * A node of a parallel solver in a federation: its peers, other sites on the links its transports give it (see [Link]), on this
 * machine or another, whatever their platform. Each peer lending ([lends]) is a [ConsumerHost] of the node, which places
 * the consumers of its graphs on it as on any other host; the consumers its peers place here run on the threads of the
 * node. Nothing more of a graph goes through a site: the [HostFrame]s, between the consumers and their store.
 *
 * A site may require a [code] of the sites linking to it ([requiresCode]): one proving it knows it is paired with it,
 * and proves it knows the token of their pairing instead from then on, linked again with no code while the two run;
 * one proving no such secret is refused, and after a few wrong codes from an address its codes are refused for a
 * while. Neither secret goes over a link (see [Pairing]), and a site linking to another with one checks that the
 * other knows it as well. Nothing but the greetings is read on a link before its site is admitted.
 */
@InternalArg2pApi
class Site(
    val node: ParallelNode,
    private val lends: Boolean = true,
) : LinkListener {
    // A peer: its name, its first link, what it offers, and, if it lends, the host the node places consumers on
    private data class Peer(
        val name: String,
        val link: Link,
        val capacity: Int?,
        val lends: Boolean,
        val host: PeerHost?,
    )

    /** The identifier of the site: that of its node */
    val id: String get() = node.id

    // Whether this site runs the consumers of others
    private val lending: Boolean get() = lends && (node.threads?.capacity ?: 0) > 0

    // The state shared by the threads of the site, as immutable values replaced under the lock: the peers by site, the
    // links of each, the consumers run here for each
    private val lock = Any()

    @Volatile private var peers: Map<String, Peer> = emptyMap()

    @Volatile private var links: Map<Link, String> = emptyMap()

    private var lent: Map<String, Set<Pair<String, Int>>> = emptyMap()

    // The tokens of the pairings made linking to each address, to link to it again with no code
    @Volatile private var tokens: Map<String, String> = emptyMap()

    // The code of the site and its pairings, and the links this site opened waiting to be greeted (told then, or told why
    // they are refused)
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

    /** The sites this one is federated with */
    val peerSites: Set<String> get() = peers.keys

    /** The name of the site, as its peers are told */
    @Volatile var name: String = ""

    /**
     * Whether the sites linking to this one must present its [code], or the token of a pairing made with it: refused
     * otherwise. The links it opens are not checked.
     */
    @Volatile var requiresCode: Boolean = false

    /**
     * The code the sites linking to this one prove they know, if it requires one: four digits, random, renewed once a
     * site pairs with this one
     */
    val code: String get() = pairing.code

    /** A new [code], the one before refused from now on (the pairings made stay) */
    fun newCode(): String = pairing.renew()

    // [code] the new code (chosen on another thread)
    internal fun newCode(code: String): String = pairing.renew(code)

    /** The sites paired with this one (one proved the code of the other): linked again with no code while they run */
    val pairedSites: Set<String> get() = pairing.paired

    // The token of the pairing with [site], to present linking to it again
    internal fun tokenOf(site: String): String? = pairing.tokenOf(site)

    // [link], opened by this site, told [done] once the site at its other end greets this one (null), refuses it or
    // closes (why)
    internal fun greeting(
        link: Link,
        done: (refusal: String?) -> Unit,
    ) = locked(lock) { greetings[link] = done }

    // What to present linking to [address]: [code] if given, or the token of a pairing made linking to it
    internal fun secretFor(
        address: String,
        code: String?,
    ): String? = code ?: tokens[address]

    // [link], opened by this site to [address], told [done] as with [greeting]: the token of a pairing it makes remembered
    internal fun linking(
        address: String,
        link: Link,
        done: (refusal: String?) -> Unit,
    ) = greeting(link) { refusal ->
        if (refusal == null) {
            val token = links[link]?.let { pairing.tokenOf(it) }
            if (token != null) locked(lock) { tokens = tokens + (address to token) }
        }
        done(refusal)
    }

    // [link] no longer waited for (see [greeting]): its waiter gave up
    internal fun forgetGreeting(link: Link) = locked(lock) { greetings.remove(link) }

    // Whether the site at the other end of [link] greeted this one, and was admitted
    internal fun admitted(link: Link): Boolean = link in links

    // The consumers run here for the graphs of other sites, for the tests
    internal val consumersRun: Int get() = locked(lock) { lent.values.sumOf { it.size } }

    /** The sites of the federation, as this site knows them: itself and its peers */
    val stats: FederationStats
        get() {
            val linked = peers.map { (site, peer) -> SiteStats(site, peer.name, peer.capacity) }
            return FederationStats(id, listOf(SiteStats(id, name, node.workers)) + linked)
        }

    /** Called whenever a site joins or leaves the peers of this one */
    var onPeersChanged: (() -> Unit)? = null

    // A peer lending, as a host of the node: its consumers reached through the frames on its link
    private inner class PeerHost(
        override val id: String,
    ) : ConsumerHost {
        // what each consumer placed on it asks of its store
        private var toStore: Map<Pair<String, Int>, (HostFrame.Call) -> Unit> = emptyMap()

        override val capacity: Int? get() = peers[id]?.capacity

        private fun send(frame: HostFrame) {
            peers[id]?.link?.send(FrameCodec.encode(Frame.Host(HostCodec.encode(frame))))
        }

        override fun start(
            start: HostFrame.Start,
            toStore: (HostFrame.Call) -> Unit,
        ) {
            locked(lock) { this.toStore = this.toStore + ((start.session to start.consumer) to toStore) }
            send(start)
        }

        override fun answer(answer: HostFrame.Answer) = send(answer)

        override fun abandon(abandon: HostFrame.Abandon) {
            locked(lock) { toStore = toStore - (abandon.session to abandon.consumer) }
            send(abandon)
        }

        // What a consumer run there asks of its store
        fun received(call: HostFrame.Call) {
            val reply = locked(lock) { toStore[call.session to call.consumer] }
            (reply ?: node::toStore)(call)
        }
    }

    // A [HostFrame] from the site at the other end of [link]: for a consumer run here, or from one run there
    private fun hostFrame(
        link: Link,
        text: String,
    ) {
        val site = links[link] ?: return
        when (val frame = HostCodec.decode(text)) {
            is HostFrame.Start -> startConsumer(site, link, frame)
            is HostFrame.Answer -> node.threads?.answer(frame)
            is HostFrame.Abandon -> {
                locked(lock) { lent = lent + (site to (lent[site].orEmpty() - (frame.session to frame.consumer))) }
                node.threads?.abandon(frame)
            }
            is HostFrame.Call -> peers[site]?.host?.received(frame) ?: node.toStore(frame)
            else -> Unit
        }
    }

    // The consumer of a graph of [site] [start] tells of, run on a thread of the node: what it asks goes back on the link,
    // and a site that does not lend tells the graph it failed, rather than leave it waiting
    private fun startConsumer(
        site: String,
        link: Link,
        start: HostFrame.Start,
    ) {
        val toStore = { call: HostFrame.Call -> sendTo(site, link, call) }
        val threads = node.threads.takeIf { lending }
        if (threads == null) {
            return toStore(
                HostFrame.Call.failing(start.session, start.consumer, "the site $id cannot run the solver of part ${start.consumer}"),
            )
        }
        locked(lock) { lent = lent + (site to (lent[site].orEmpty() + (start.session to start.consumer))) }
        threads.start(start, toStore)
    }

    // (on the first link to [site], if the one [link] closed)
    private fun sendTo(
        site: String,
        link: Link,
        frame: HostFrame,
    ) {
        (peers[site]?.link ?: link).send(FrameCodec.encode(Frame.Host(HostCodec.encode(frame))))
    }

    // Every link begins with the greetings of its ends: on a link it opened, this site greets the other once it spoke
    // first (challenging it, or greeting it); on another link, it challenges the other first, with a nonce to prove its
    // secret on (see [Pairing]), and greets it once greeted
    override fun opened(link: Link) {
        val nonce = locked(lock) { openingOf(link).nonce }
        when {
            link.outgoing || link in links -> Unit
            else -> link.send(FrameCodec.encode(Frame.Challenge(nonce)))
        }
    }

    // Whether a site linking to this one on [link] must prove it knows its code
    private fun guarded(link: Link) = requiresCode && !link.outgoing

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
            is Frame.Host -> hostFrame(link, decoded.frame)
        }
    }

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
        link.send(hello(nonce, proof, token))
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
        val (gone, host) =
            locked(lock) {
                val site = links[link]
                links = links - link
                openings -= link
                val replacement = links.entries.firstOrNull { it.value == site }?.key
                when {
                    site == null || peers[site]?.link != link -> null to null
                    replacement != null -> {
                        peers = peers + (site to peers.getValue(site).copy(link = replacement))
                        null to null
                    }
                    else -> {
                        val host = peers.getValue(site).host
                        peers = peers - site
                        site to host
                    }
                }
            }
        gone ?: return
        host?.let { node.removeHost(it) }
        forget(gone)
        onPeersChanged?.invoke()
    }

    // The consumers run here for the graphs of [site], which left, given up
    private fun forget(site: String) {
        val given = locked(lock) { lent[site].orEmpty().also { lent = lent - site } }
        given.forEach { (session, consumer) ->
            node.threads?.abandon(HostFrame.Abandon(session, consumer, "the graph of $site is no longer built here"))
        }
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
                    if (joining) {
                        val host = if (hello.lends) PeerHost(hello.site) else null
                        peers = peers + (hello.site to Peer(hello.name, link, hello.capacity, hello.lends, host))
                    }
                }
            }
        greet(link, proven?.let { known -> hello.nonce?.let { Pairing.proof(known.secret, id, it) } }, proven?.token)
        locked(lock) { greetings.remove(link) }?.invoke(null)
        if (joined) {
            peers[hello.site]?.host?.let { node.addHost(it) }
            onPeersChanged?.invoke()
        }
    }

    // The greeting of a peer again on [link]: what it offers updated; a greeting of another site closes the link, which
    // stays that of the site first greeted on it
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

    // The greeting of this site (see [Frame.Hello])
    private fun hello(
        nonce: String? = null,
        proof: String? = null,
        token: String? = null,
    ): String = FrameCodec.encode(Frame.Hello(id, VERSION, node.workers, lending, nonce, proof, token, name))

    companion object {
        /** The version a site tells its peers: sites of different versions do not federate */
        const val VERSION: String = "arg2p-federation/1 $ARG2P_VERSION"
    }
}
