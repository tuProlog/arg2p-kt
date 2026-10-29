package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.argumentation.parallel.cluster.ClusterSolver
import it.unibo.tuprolog.argumentation.parallel.cluster.ThreadedCluster
import it.unibo.tuprolog.argumentation.parallel.federation.MemoryLinks
import kotlin.test.AfterTest
import kotlin.test.assertEquals

// The scenarios on the threads of the JVM, the rules spread over three partitions
class ThreadedClusterSolverTest : ClusterSolverScenarios() {
    private val clusters = mutableListOf<ThreadedCluster>()

    override fun cluster(
        theory: String,
        flags: Flags,
    ): ClusterSolver =
        ThreadedCluster(threads = 3, flags = flags)
            .also {
                clusters += it
                it.load(theory)
            }.solver

    override fun configure(flags: Flags) {
        clusters.last().flags = flags
    }

    @AfterTest
    fun close() = clusters.forEach { it.close() }
}

// The scenarios on two federated clusters of the JVM, the graph built by the workers of both
class FederatedClusterSolverTest : ClusterSolverScenarios() {
    private val clusters = mutableListOf<ThreadedCluster>()

    override fun cluster(
        theory: String,
        flags: Flags,
    ): ClusterSolver {
        val solving = ThreadedCluster(threads = 1, parts = 2, flags = flags, lends = false)
        val lending = ThreadedCluster(threads = 2)
        clusters += listOf(solving, lending)
        MemoryLinks.connect(solving.site, lending.site)
        solving.load(theory)
        return solving.solver
    }

    // the flags of the cluster solving the goals, its peer kept
    override fun configure(flags: Flags) {
        val (solving, lending) = clusters.takeLast(2)
        solving.flags = flags
        assertEquals(setOf(lending.id), solving.peers)
    }

    @AfterTest
    fun close() = clusters.forEach { it.close() }
}
