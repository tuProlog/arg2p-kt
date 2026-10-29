package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.parallel.cluster.ThreadedCluster
import it.unibo.tuprolog.argumentation.parallel.federation.MemoryLinks
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull
import kotlin.test.assertTrue

// The statistics of two federated clusters of the JVM, each on threads of its own
class FederationStatsTest {
    // a chain whose last conclusion is rebutted by an argument resting on another chain
    private val theory =
        (1..6).joinToString("\n") { "r$it : a${it - 1} => a$it." } +
            "\nf0 :=> a0.\n" +
            (1..6).joinToString("\n") { "s$it : b${it - 1} => b$it." } +
            "\ng0 :=> b0.\nt : b6 => -a6.\nsup(t, r6).\n"

    @Test
    fun theSitesAndTheWorkersOfAFederation() {
        ThreadedCluster(threads = 1, parts = 3).use { evaluating ->
            ThreadedCluster(threads = 2).use { lending ->
                var changes = 0
                evaluating.onPeersChanged = { changes++ }
                MemoryLinks.connect(evaluating.site, lending.site)
                assertEquals(1, changes)
                val stats = evaluating.stats
                assertEquals(evaluating.id, stats.site)
                assertEquals(listOf(evaluating.id to 1, lending.id to 2), stats.sites.map { it.id to it.workers })
                assertTrue(stats.sites.none { it.delegate || it.via != null })
                assertNull(stats.evaluation)
                evaluating.load(theory)
                assertEquals(listOf("a6"), evaluating.solve("a6").outArgs.map { it.claim })
                // the workers placed in proportion to those of each site
                val evaluation = evaluating.stats.evaluation!!
                assertEquals(listOf(evaluating.id, lending.id, lending.id), evaluation.workers.map { it.site })
                assertEquals(evaluation.added, evaluation.workers.sumOf { it.arguments })
                assertTrue(evaluation.workers.sumOf { it.attacks } > 0)
                assertTrue(evaluation.milliseconds > 0)
            }
        }
    }
}
