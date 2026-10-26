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
    fun framesAreReadBackAsWritten() {
        val frames =
            listOf(
                Frame.Hello("site-1", Site.VERSION, null, false),
                Frame.Hello("site-2", Site.VERSION, 8, true),
                Frame.Placement("site-1#3", mapOf("worker_0" to "site-1", "worker_1" to "site-2")),
                Frame.Lost("site-1#3"),
                Frame.Deliver("site-1#3", "worker_1", "3:Ack2:-1"),
            )
        frames.forEach { assertEquals(it, FrameCodec.decode(FrameCodec.encode(it))) }
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
