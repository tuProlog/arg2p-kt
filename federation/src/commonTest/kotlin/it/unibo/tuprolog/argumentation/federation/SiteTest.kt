package it.unibo.tuprolog.argumentation.federation

import it.unibo.tuprolog.argumentation.parallel.ParallelNode
import it.unibo.tuprolog.argumentation.parallel.consumer.ConsumerStart
import it.unibo.tuprolog.argumentation.parallel.host.ConsumerThreads
import it.unibo.tuprolog.argumentation.parallel.host.HostCodec
import it.unibo.tuprolog.argumentation.parallel.host.HostFrame
import it.unibo.tuprolog.argumentation.parallel.host.HostedThread
import it.unibo.tuprolog.argumentation.parallel.host.SequentialScheduler
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotEquals
import kotlin.test.assertNotNull
import kotlin.test.assertNull
import kotlin.test.assertTrue
import kotlin.time.Duration.Companion.seconds
import kotlin.time.TestTimeSource

class SiteTest {
    @Test
    fun sitesKnowEachOther() {
        val scheduler = SequentialScheduler()
        val first = site(scheduler, null)
        val second = site(scheduler, null)
        MemoryLinks.connect(first, second, scheduler)
        assertEquals(setOf(second.id), first.peerSites)
        assertEquals(setOf(first.id), second.peerSites)
    }

    // A peer answers the beats of a site as long as it handles frames; one that hangs with its link open is let go once
    // silent past the deadline, as if it left
    @Test
    fun aPeerSilentPastTheDeadlineIsLetGo() {
        val scheduler = SequentialScheduler()
        val clock = TestTimeSource()
        val first = Site(ParallelNode(scheduler, null), deadline = 5.seconds, clock = clock)
        val second = site(scheduler, null)
        var hung = false
        // second, unless hung: then it handles no frame, its link open
        val hanging =
            object : LinkListener by second {
                override fun received(
                    link: Link,
                    frame: String,
                ) {
                    if (!hung) second.received(link, frame)
                }
            }
        MemoryLinks.connect(first, hanging, scheduler)
        assertEquals(setOf(second.id), first.peerSites)
        clock += 4.seconds
        first.heartbeat()
        clock += 4.seconds
        first.heartbeat()
        assertEquals(setOf(second.id), first.peerSites, "a peer answering is kept")
        hung = true
        clock += 4.seconds
        first.heartbeat()
        assertEquals(setOf(second.id), first.peerSites, "silent for 4 seconds")
        clock += 2.seconds
        first.heartbeat()
        assertTrue(first.peerSites.isEmpty(), "silent for 6 seconds")
    }

    @Test
    fun aSiteOfAnotherVersionIsRefused() {
        val scheduler = SequentialScheduler()
        val site = site(scheduler, null)
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

    // A site tells it runs the consumers of others if it has threads for them, and it lends: no consumer is placed on it otherwise
    @Test
    fun aSiteLendsItsThreadsAlone() {
        val told = { threads: ConsumerThreads?, lends: Boolean ->
            val scheduler = SequentialScheduler()
            val site = site(scheduler, 1, threads = threads, lends = lends)
            val frames = mutableListOf<Frame>()
            val other =
                object : LinkListener {
                    override fun opened(link: Link) = link.send(FrameCodec.encode(Frame.Hello("other", Site.VERSION, 1, true)))

                    override fun received(
                        link: Link,
                        frame: String,
                    ) {
                        frames += FrameCodec.decode(frame)
                    }

                    override fun closed(link: Link) = Unit
                }
            MemoryLinks.connect(site, other, scheduler)
            frames.filterIsInstance<Frame.Hello>().single().lends
        }
        val thread = { ConsumerThreads("here", 1, listOf(HostedThread {})) { HostedThread {} } }
        assertEquals(false, told(null, true))
        assertEquals(true, told(thread(), true))
        assertEquals(false, told(thread(), false))
        assertEquals(false, told(ConsumerThreads("here", 0) { HostedThread {} }, true))
    }

    @Test
    fun framesAreReadBackAsWritten() {
        val frames =
            listOf(
                Frame.Hello("site-1", Site.VERSION, null, false),
                Frame.Hello("site-2", Site.VERSION, 8, true),
                Frame.Hello("site-3", Site.VERSION, 1, true, name = "desk"),
                Frame.Hello("site-4", Site.VERSION, 2, true, nonce = "a1", proof = "b2", token = "c3"),
                Frame.Hello("site-5", Site.VERSION, 2, true, nonce = "a1"),
                Frame.Challenge("a1"),
                Frame.Refused("Wrong code"),
                Frame.Host("anything"),
            )
        frames.forEach { assertEquals(it, FrameCodec.decode(FrameCodec.encode(it))) }
    }

    @Test
    fun whatGoesBetweenConsumersAndStoresIsReadBackAsWritten() {
        val frames =
            listOf(
                HostFrame.Start("site-1#3", 2, ConsumerStart("a :- b.", "queryMode(true).", "context_reset", setOf("a/0", "-b/1"), true)),
                HostFrame.Start("site-1#3", 0, ConsumerStart("", "", "true", null, false)),
                HostFrame.Call("site-1#3", 1, 7, "barrier", "3"),
                HostFrame.Answer("site-1#3", 1, 7, "0:1:0"),
                HostFrame.Abandon("site-1#3", 1, "gone"),
                HostFrame.Done,
                HostFrame.Stop,
            )
        frames.forEach { assertEquals(it, HostCodec.decode(HostCodec.encode(it))) }
    }

    // A site linking with no greeting, to a site requiring a code or not: what it sends is not read, and its link closed
    @Test
    fun nothingButTheGreetingsIsReadBeforeASiteIsAdmitted() {
        listOf(true, false).forEach { requiresCode ->
            val scheduler = SequentialScheduler()
            val site = site(scheduler, 1).also { it.requiresCode = requiresCode }
            var closed = false
            val stranger =
                object : LinkListener {
                    // a consumer started on the site
                    override fun opened(link: Link) {
                        val start = HostFrame.Start("stranger#1", 1, ConsumerStart("a.", "", "true", null, false))
                        link.send(FrameCodec.encode(Frame.Host(HostCodec.encode(start))))
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
            assertEquals(0, site.consumersRun)
            assertTrue(site.peerSites.isEmpty())
        }
    }

    @Test
    fun aLinkStaysThatOfTheSiteFirstGreetedOnIt() {
        val scheduler = SequentialScheduler()
        val site = site(scheduler, 1)
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
        val accepting = site(scheduler, 1).also { it.requiresCode = true }
        val connecting = site(scheduler, 1)
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
            val spoofer = site(scheduler, 1, accepting.id).also { it.requiresCode = requiresCode }
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
        val accepting = site(scheduler, 1).also { it.requiresCode = true }
        val connecting = site(scheduler, 1)
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
}
