package it.unibo.tuprolog.argumentation.parallel.federation

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

// The sites heard on the local network, and the links opened to them
class BeaconsTest {
    private val interval = 1_000L
    private val scheduler = SequentialScheduler()

    // The beacon of a site [id]
    private fun beaconOf(id: String) = Beacons(Site(scheduler, 1, id = id), 9_000, interval, { false }) { _, _, _ -> }.beacon

    @Test
    fun aSiteClaimedFromTwoHostsIsNeitherNearbyNorLinkedTo() {
        val site = Site(scheduler, 1, id = "site-a")
        val connected = mutableListOf<String>()
        val beacons =
            Beacons(site, 9_000, interval, { it == "10.0.0.9" || it == "127.0.0.1" }) { url, _, done ->
                connected +=
                    url.also { done() }
            }
        val other = beaconOf("site-b")
        // this host, from two of its addresses: one site
        beacons.heard(other, "127.0.0.1", 0)
        beacons.heard(other, "10.0.0.9", 100)
        assertEquals(listOf("site-b"), site.nearby.map { it.id })
        assertTrue(connected.isNotEmpty())
        connected.clear()
        // another host claiming it as well
        beacons.heard(other, "10.0.0.5", 200)
        assertTrue(site.nearby.isEmpty())
        beacons.heard(other, "10.0.0.9", 2_000)
        assertTrue(connected.isEmpty())
        // the other host heard no more: the site again
        beacons.heard(other, "10.0.0.9", 4_000)
        assertEquals(listOf("site-b"), site.nearby.map { it.id })
        assertTrue(connected.isNotEmpty())
    }

    // the site with the higher identifier waits for the other to link; linked, it waits as much again once the link
    // closes, rather than linking at once as the other does
    @Test
    fun aPeerLeavingIsWaitedForAsOnceHeard() {
        val site = Site(scheduler, 1, id = "site-z")
        val connected = mutableListOf<Long>()
        var now = 0L
        val beacons = Beacons(site, 9_000, interval, { false }) { _, _, done -> connected += now.also { done() } }
        val other = Site(scheduler, 1, id = "site-a")
        val beacon = beaconOf(other.id)
        beacons.heard(beacon, "10.0.0.2", now)
        val links = mutableListOf<Link>()
        MemoryLinks.connect(
            object : LinkListener by other {
                override fun opened(link: Link) {
                    links += link
                    other.opened(link)
                }
            },
            site,
            scheduler,
        )
        assertEquals(setOf(other.id), site.peerSites)
        now = 10 * interval
        beacons.heard(beacon, "10.0.0.2", now)
        links.single().close()
        assertTrue(site.peerSites.isEmpty())
        now = 100 * interval
        beacons.heard(beacon, "10.0.0.2", now)
        assertTrue(connected.isEmpty())
        now = 104 * interval
        beacons.heard(beacon, "10.0.0.2", now)
        assertEquals(listOf(now), connected)
    }
}
