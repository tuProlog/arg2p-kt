package it.unibo.tuprolog.argumentation.parallel.federation

import it.unibo.tuprolog.argumentation.parallel.cluster.LocalCluster
import it.unibo.tuprolog.argumentation.parallel.message.Ack
import it.unibo.tuprolog.argumentation.parallel.message.MessageCodec
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotEquals
import kotlin.test.assertNotNull
import kotlin.test.assertNull
import kotlin.test.assertTrue

class SiteTest {
    // a chain whose last conclusion is rebutted by an argument resting on another chain
    private val theory =
        (1..6).joinToString("\n") { "r$it : a${it - 1} => a$it." } +
            "\nf0 :=> a0.\n" +
            (1..6).joinToString("\n") { "s$it : b${it - 1} => b$it." } +
            "\ng0 :=> b0.\nt : b6 => -a6.\nsup(t, r6).\n"

    @Test
    fun sitesKnowEachOther() {
        val scheduler = SequentialScheduler()
        val first = LocalCluster(scheduler = scheduler)
        val second = LocalCluster(scheduler = scheduler)
        MemoryLinks.connect(first.site, second.site, scheduler)
        assertEquals(setOf(second.site.id), first.site.peerSites)
        assertEquals(setOf(first.site.id), second.site.peerSites)
    }

    @Test
    fun aSiteOfAnotherVersionIsRefused() {
        val scheduler = SequentialScheduler()
        val site = LocalCluster(scheduler = scheduler).site
        var closed = false
        val stranger =
            object : LinkListener {
                override fun opened(link: Link) = link.send(FrameCodec.encode(Frame.Hello("stranger", "other", 1, true)))

                override fun received(
                    link: Link,
                    frame: String,
                ) = Unit

                override fun closed(link: Link) {
                    closed = true
                }
            }
        MemoryLinks.connect(site, stranger, scheduler)
        assertTrue(closed)
        assertTrue(site.peerSites.isEmpty())
    }

    @Test
    fun theGraphIsBuiltAgainWhenASiteLeavesWhileItIsBuilt() {
        val scheduler = SequentialScheduler()
        val evaluating = LocalSite(0, scheduler)
        val leaving = LocalSite(2, scheduler)
        val staying = LocalSite(2, scheduler)
        // the leaving site drops its link to the evaluating one after a few of its messages
        MemoryLinks.connect(evaluating.site, Leaving(leaving.site, after = 3), scheduler)
        MemoryLinks.connect(evaluating.site, staying.site, scheduler)
        MemoryLinks.connect(leaving.site, staying.site, scheduler)
        evaluating.load(theory)
        val response = evaluating.solve("a6")
        assertEquals(setOf(staying.site.id), evaluating.site.peerSites)
        assertEquals(listOf("a6"), response.outArgs.map { it.claim })
        assertEquals(listOf("b6"), evaluating.solve("b6").inArgs.map { it.claim })
    }

    @Test
    fun theGraphIsBuiltAgainWhenTheLinkBetweenTwoOfItsSitesCloses() {
        val scheduler = SequentialScheduler()
        val evaluating = LocalSite(0, scheduler)
        val first = LocalSite(2, scheduler)
        val second = LocalSite(2, scheduler)
        MemoryLinks.connect(evaluating.site, first.site, scheduler)
        MemoryLinks.connect(evaluating.site, second.site, scheduler)
        // the link between the two closes after the first message between their workers
        MemoryLinks.connect(Leaving(first.site, after = 1), second.site, scheduler)
        evaluating.load(theory)
        assertEquals(listOf("a6"), evaluating.solve("a6").outArgs.map { it.claim })
        assertEquals(setOf(first.site.id, second.site.id), evaluating.site.peerSites)
        assertEquals(setOf(evaluating.site.id), first.site.peerSites)
    }

    @Test
    fun theWorkersPlacedOnASiteRunOnItsDelegates() {
        val scheduler = SequentialScheduler()
        val evaluating = LocalSite(0, scheduler)
        val lending = LocalSite(0, scheduler)
        val delegates = List(2) { LocalSite(1, scheduler) }
        delegates.forEach { MemoryLinks.delegate(lending.site, it.site, scheduler) }
        MemoryLinks.connect(evaluating.site, lending.site, scheduler)
        evaluating.load(theory)
        assertEquals(listOf("a6"), evaluating.solve("a6").outArgs.map { it.claim })
        assertEquals(0, evaluating.site.workersRun)
        assertEquals(0, lending.site.workersRun)
        assertEquals(listOf(1, 1), delegates.map { it.site.workersRun })
        // the delegates are no peers of the evaluating site, their workers are offered by theirs
        assertEquals(setOf(lending.site.id), evaluating.site.peerSites)
    }

    @Test
    fun sitesOpeningLinksOfTheirOwnAreIntroducedAndSignalThroughTheirPeer() {
        val scheduler = SequentialScheduler()
        val hub = Site(scheduler, workers = 1)
        val (first, second) = List(2) { Site(scheduler, workers = 1, introducible = true) }.sortedBy { it.id }
        val signals = mutableListOf<String>()
        // the first, of lower identifier, asks for a link; the second opens it
        first.onIntroduced = { first.signal(it, "open") }
        second.onSignal = { from, payload ->
            signals += "$payload from ${if (from == first.id) "first" else from}"
            MemoryLinks.connect(second, first, scheduler)
        }
        MemoryLinks.connect(hub, first, scheduler)
        MemoryLinks.connect(hub, second, scheduler)
        assertEquals(listOf("open from first"), signals)
        assertEquals(setOf(hub.id, second.id), first.peerSites)
        // a site that does not open links of its own is introduced to none
        assertTrue(Site(scheduler, workers = 1).also { MemoryLinks.connect(hub, it, scheduler) }.peerSites == setOf(hub.id))
    }

    @Test
    fun framesAreReadBackAsWritten() {
        val frames =
            listOf(
                Frame.Hello("site-1", Site.VERSION, null, false),
                Frame.Hello("site-2", Site.VERSION, 8, true),
                Frame.Placement("site-1#3", mapOf("worker_0" to "site-1", "worker_1" to "site-2")),
                Frame.Lost("site-1#3"),
                Frame.Forget("site-2"),
                Frame.Hello("site-3", Site.VERSION, 1, true, introducible = true),
                Frame.Hello("site-4", Site.VERSION, 2, true, nonce = "a1", proof = "b2", token = "c3"),
                Frame.Hello("site-5", Site.VERSION, 2, true, nonce = "a1"),
                Frame.Challenge("a1"),
                Frame.Refused("Wrong code"),
                Frame.Introduce("site-3"),
                Frame.Signal("site-3", "site-1", "{\"description\": 1}"),
                Frame.Deliver("site-1#3", "worker_1", "3:Ack2:-1"),
            )
        frames.forEach { assertEquals(it, FrameCodec.decode(FrameCodec.encode(it))) }
    }

    // A lending site leaving once a graph is built: a later evaluation needing more of it builds it again with the sites
    // left, rather than waiting for the workers of the one that left
    @Test
    fun theGraphIsBuiltAgainWhenASiteLeavesOnceItIsBuilt() {
        val scheduler = SequentialScheduler()
        val evaluating = LocalSite(0, scheduler)
        val leaving = Closable(LocalSite(2, scheduler).site)
        val staying = LocalSite(2, scheduler)
        MemoryLinks.connect(evaluating.site, leaving, scheduler)
        MemoryLinks.connect(evaluating.site, staying.site, scheduler)
        evaluating.load("$theory\nu0 :=> c0.\nu1 : c0 => c1.\n")
        assertEquals(listOf("a6"), evaluating.solve("a6").outArgs.map { it.claim })
        leaving.close()
        assertEquals(setOf(staying.site.id), evaluating.site.peerSites)
        assertEquals(listOf("c1"), evaluating.solve("c1").inArgs.map { it.claim })
    }

    // A site linking with no greeting, to a site requiring a code or not: what it sends is not read, and its link closed
    @Test
    fun nothingButTheGreetingsIsReadBeforeASiteIsAdmitted() {
        listOf(true, false).forEach { requiresCode ->
            val scheduler = SequentialScheduler()
            val site = Site(scheduler, 1).also { it.requiresCode = requiresCode }
            var closed = false
            val stranger =
                object : LinkListener {
                    // the workers of a session placed on the site, and a message for one of them
                    override fun opened(link: Link) {
                        link.send(FrameCodec.encode(Frame.Placement("stranger#1", mapOf("worker_0" to site.id))))
                        link.send(FrameCodec.encode(Frame.Deliver("stranger#1", "worker_0", MessageCodec.encode(Ack(-1)))))
                    }

                    override fun received(
                        link: Link,
                        frame: String,
                    ) = Unit

                    override fun closed(link: Link) {
                        closed = true
                    }
                }
            MemoryLinks.connect(Ends(site, address = "10.0.0.2"), stranger, scheduler)
            assertTrue(closed)
            assertEquals(0, site.workersRun)
            assertTrue(site.peerSites.isEmpty())
        }
    }

    @Test
    fun aLinkStaysThatOfTheSiteFirstGreetedOnIt() {
        val scheduler = SequentialScheduler()
        val site = Site(scheduler, 1)
        val peers = mutableListOf<Set<String>>()
        site.onPeersChanged = { peers += site.peerSites }
        var closed = false
        val impostor =
            object : LinkListener {
                override fun opened(link: Link) = Unit

                // greeted as a site, then as another
                override fun received(
                    link: Link,
                    frame: String,
                ) {
                    if (FrameCodec.decode(frame) !is Frame.Challenge) return
                    link.send(FrameCodec.encode(Frame.Hello("site-a", Site.VERSION, 1, true)))
                    link.send(FrameCodec.encode(Frame.Hello("site-b", Site.VERSION, 1, true)))
                }

                override fun closed(link: Link) {
                    closed = true
                }
            }
        MemoryLinks.connect(site, impostor, scheduler)
        assertTrue(closed)
        // a peer as the first, until the link closed
        assertEquals(listOf(setOf("site-a"), emptySet()), peers)
    }

    // A site pairs with one requiring a code by proving it knows the code, then links again proving it knows their
    // token: neither secret goes over a link (the token alone, once, to the site paired), and a site claiming the
    // identifier of the other cannot prove it knows the token
    @Test
    fun theCodeAndTheTokensAreProvenNeverSent() {
        val scheduler = SequentialScheduler()
        val accepting = Site(scheduler, 1).also { it.requiresCode = true }
        val connecting = Site(scheduler, 1)
        val code = accepting.code
        val pairing = mutableListOf<String>()
        MemoryLinks.connect(Ends(connecting, pairing, code = code), Ends(accepting, pairing, address = "10.0.0.2"), scheduler)
        assertEquals(setOf(accepting.id), connecting.peerSites)
        assertEquals(setOf(connecting.id), accepting.peerSites)
        assertEquals(setOf(accepting.id), connecting.pairedSites)
        assertEquals(setOf(connecting.id), accepting.pairedSites)
        assertNotEquals(code, accepting.code)
        val hellos = pairing.map { FrameCodec.decode(it) }.filterIsInstance<Frame.Hello>()
        assertTrue(hellos.none { code in listOfNotNull(it.nonce, it.proof, it.token) })
        val token = assertNotNull(connecting.tokenOf(accepting.id))
        assertEquals(listOf(token), hellos.mapNotNull { it.token })
        // linked again, by the token: a second link to the same site
        val again = mutableListOf<String>()
        MemoryLinks.connect(Ends(connecting, again, code = token), Ends(accepting, again, address = "10.0.0.3"), scheduler)
        assertEquals(setOf(accepting.id), connecting.peerSites)
        assertTrue(again.none { token in it })
        // a site claiming the identifier of the one paired: it proves nothing, and the link is closed
        listOf(false, true).forEach { requiresCode ->
            val spoofer = Site(scheduler, 1, id = accepting.id).also { it.requiresCode = requiresCode }
            val spoofed = mutableListOf<String>()
            var refusal: String? = null
            val linking = Ends(connecting, spoofed, code = token) { link -> connecting.greeting(link) { refusal = it } }
            MemoryLinks.connect(linking, Ends(spoofer, spoofed, address = "10.0.0.4"), scheduler)
            assertTrue(spoofed.none { token in it })
            assertNotNull(refusal)
            assertTrue(spoofer.peerSites.isEmpty())
            assertTrue(spoofer.pairedSites.isEmpty())
        }
        assertEquals(setOf(accepting.id), connecting.peerSites)
    }

    @Test
    fun aWrongCodeIsRefusedAndTheSiteTold() {
        val scheduler = SequentialScheduler()
        val accepting = Site(scheduler, 1).also { it.requiresCode = true }
        val connecting = Site(scheduler, 1)
        var refusal: String? = null
        val wrong = if (accepting.code == "0000") "0001" else "0000"
        val linking = Ends(connecting, code = wrong) { link -> connecting.greeting(link) { refusal = it } }
        MemoryLinks.connect(linking, Ends(accepting, address = "10.0.0.2"), scheduler)
        assertEquals("Wrong code", refusal)
        assertTrue(accepting.peerSites.isEmpty())
        assertTrue(connecting.peerSites.isEmpty())
        // with no address, no code is checked
        MemoryLinks.connect(Ends(connecting, code = accepting.code), accepting, scheduler)
        assertTrue(accepting.peerSites.isEmpty())
        assertTrue(accepting.pairedSites.isEmpty())
        assertNull(connecting.tokenOf(accepting.id))
    }

    // [site] as a transport shows it its links: links it opened, presenting [code], if given; links from [address]
    // otherwise. The frames it receives go to [wire], and the links to [opening] before they open.
    private class Ends(
        private val site: Site,
        private val wire: MutableList<String> = mutableListOf(),
        private val code: String? = null,
        private val address: String? = null,
        private val opening: (Link) -> Unit = {},
    ) : LinkListener {
        private val ends = mutableMapOf<Link, Link>()

        private fun end(link: Link): Link =
            ends.getOrPut(link) {
                object : Link by link {
                    override val outgoing: Boolean get() = this@Ends.code != null
                    override val code: String? get() = this@Ends.code
                    override val address: String? get() = this@Ends.address
                }.also(opening)
            }

        override fun opened(link: Link) = site.opened(end(link))

        override fun received(
            link: Link,
            frame: String,
        ) {
            wire += frame
            site.received(end(link), frame)
        }

        override fun closed(link: Link) = site.closed(end(link))
    }

    // A site whose links can be closed
    private class Closable(
        private val site: Site,
    ) : LinkListener by site {
        private val links = mutableListOf<Link>()

        override fun opened(link: Link) {
            links += link
            site.opened(link)
        }

        fun close() = links.toList().forEach { it.close() }
    }

    // A site whose link closes after it received [after] messages for its workers
    private class Leaving(
        private val site: Site,
        private var after: Int,
    ) : LinkListener by site {
        override fun received(
            link: Link,
            frame: String,
        ) {
            if (FrameCodec.decode(frame) is Frame.Deliver && --after == 0) return link.close()
            site.received(link, frame)
        }
    }
}
