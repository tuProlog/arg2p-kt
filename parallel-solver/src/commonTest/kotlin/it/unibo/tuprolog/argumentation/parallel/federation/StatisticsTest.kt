package it.unibo.tuprolog.argumentation.parallel.federation

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.cluster.LocalCluster
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull
import kotlin.test.assertTrue

// What a site knows of its federation: the sites, how it reaches them, and the evaluations it answered
class StatisticsTest {
    // two parts no attack relates: a and b, and c, d and -d
    private val theory = "r1 :=> a.\nr2 : a => b.\nr3 :=> c.\nr4 : c => d.\nr5 :=> -d.\n"

    @Test
    fun theDelegatesOfASite() {
        val scheduler = SequentialScheduler()
        val evaluating = LocalCluster(parts = 0, scheduler = scheduler)
        val lending = LocalCluster(parts = 0, scheduler = scheduler)
        val delegates = List(2) { LocalCluster(parts = 1, scheduler = scheduler) }
        delegates.forEach { MemoryLinks.delegate(lending.site, it.site, scheduler) }
        MemoryLinks.connect(evaluating.site, lending.site, scheduler)
        // the lending site offers the workers of its delegates, as one site
        assertEquals(listOf(evaluating.site.id to 0, lending.site.id to 2), evaluating.stats.sites.map { it.id to it.workers })
        val known = lending.stats.sites.associateBy { it.id }
        assertEquals(setOf(lending.site.id, evaluating.site.id) + delegates.map { it.site.id }, known.keys)
        assertTrue(delegates.all { known.getValue(it.site.id).delegate })
        assertTrue(known.values.all { it.via == null })
    }

    @Test
    fun aSiteReachedThroughAnother() {
        val scheduler = SequentialScheduler()
        val evaluating = LocalCluster(parts = 0, scheduler = scheduler)
        val first = LocalCluster(parts = 2, scheduler = scheduler)
        val second = LocalCluster(parts = 1, scheduler = scheduler)
        MemoryLinks.connect(evaluating.site, first.site, scheduler)
        MemoryLinks.connect(evaluating.site, second.site, scheduler)
        evaluating.load(theory)
        evaluating.solve("b")
        // the two run workers of the same graph, with no link between them
        val second2 = first.stats.sites.single { it.id == second.site.id }
        assertEquals(evaluating.site.id, second2.via)
        assertNull(second2.workers)
    }

    @Test
    fun theArgumentsBuiltForEachEvaluation() {
        val cluster = LocalCluster(parts = 2, flags = Flags.defaults().graphExtensions())
        assertNull(cluster.stats.evaluation)
        cluster.load(theory)
        cluster.solve("b")
        val first = cluster.stats.evaluation!!
        assertEquals("directional", first.locality)
        assertEquals(2, first.workers.size)
        assertEquals(listOf(cluster.site.id), first.workers.map { it.site }.distinct())
        assertEquals(2, first.added)
        assertEquals(first.added, first.workers.sumOf { it.arguments })
        assertEquals(2, first.arguments)
        // query mode: a later evaluation explores further, one it covers builds nothing
        cluster.solve("d")
        assertEquals(3, cluster.stats.evaluation!!.added)
        assertEquals(5, cluster.stats.evaluation!!.arguments)
        assertEquals(2, cluster.stats.evaluation!!.attacks)
        cluster.solve("a")
        assertEquals(0, cluster.stats.evaluation!!.added)
        // the whole graph, for a goal asking for it
        cluster.solver.solve("buildLabelSets(In, Out, Und)").next()
        assertEquals("whole_graph", cluster.stats.evaluation!!.locality)
        assertEquals(5, cluster.stats.evaluation!!.added)
    }
}
