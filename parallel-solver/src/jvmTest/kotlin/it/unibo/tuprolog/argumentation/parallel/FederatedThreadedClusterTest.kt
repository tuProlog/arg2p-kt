package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.cluster.ThreadedCluster
import it.unibo.tuprolog.argumentation.parallel.federation.MemoryLinks
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import kotlin.test.AfterTest

// The scenarios on three federated clusters of the JVM, each on threads of its own, their links delivering on the
// threads of the senders: the sites run in parallel, as on three machines
class FederatedThreadedClusterTest : EquivalenceScenarios() {
    private val clusters = mutableListOf<ThreadedCluster>()

    override fun distributed(
        theory: String,
        flags: Flags,
    ): (String) -> EvaluationResponse {
        val (evaluating, first, second) =
            listOf(ThreadedCluster(threads = 1, flags = flags), ThreadedCluster(threads = 2), ThreadedCluster(threads = 2))
        clusters += listOf(evaluating, first, second)
        MemoryLinks.connect(evaluating.site, first.site)
        MemoryLinks.connect(evaluating.site, second.site)
        MemoryLinks.connect(first.site, second.site)
        evaluating.load(theory)
        return evaluating::solve
    }

    @AfterTest
    fun close() = clusters.forEach { it.close() }
}
