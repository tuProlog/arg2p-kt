package it.unibo.tuprolog.argumentation.parallel.federation

import it.unibo.tuprolog.argumentation.parallel.cluster.LocalCluster
import kotlin.test.Test
import kotlin.test.assertEquals
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
        val evaluating = LocalCluster(parts = 0, scheduler = scheduler)
        val leaving = LocalCluster(parts = 2, scheduler = scheduler)
        val staying = LocalCluster(parts = 2, scheduler = scheduler)
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
        val evaluating = LocalCluster(parts = 0, scheduler = scheduler)
        val first = LocalCluster(parts = 2, scheduler = scheduler)
        val second = LocalCluster(parts = 2, scheduler = scheduler)
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
        val evaluating = LocalCluster(parts = 0, scheduler = scheduler)
        val lending = LocalCluster(parts = 0, scheduler = scheduler)
        val delegates = List(2) { LocalCluster(parts = 1, scheduler = scheduler) }
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
                Frame.Hello("site-4", Site.VERSION, 2, true, code = "0042"),
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
        val evaluating = LocalCluster(parts = 0, scheduler = scheduler)
        val leaving = Closable(LocalCluster(parts = 2, scheduler = scheduler).site)
        val staying = LocalCluster(parts = 2, scheduler = scheduler)
        MemoryLinks.connect(evaluating.site, leaving, scheduler)
        MemoryLinks.connect(evaluating.site, staying.site, scheduler)
        evaluating.load("$theory\nu0 :=> c0.\nu1 : c0 => c1.\n")
        assertEquals(listOf("a6"), evaluating.solve("a6").outArgs.map { it.claim })
        leaving.close()
        assertEquals(setOf(staying.site.id), evaluating.site.peerSites)
        assertEquals(listOf("c1"), evaluating.solve("c1").inArgs.map { it.claim })
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
