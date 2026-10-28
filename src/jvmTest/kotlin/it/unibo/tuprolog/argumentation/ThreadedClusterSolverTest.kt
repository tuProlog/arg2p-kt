package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.argumentation.parallel.cluster.ClusterSolver
import it.unibo.tuprolog.argumentation.parallel.cluster.ThreadedCluster
import it.unibo.tuprolog.argumentation.parallel.federation.MemoryLinks
import kotlin.test.AfterTest

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

    @AfterTest
    fun close() = clusters.forEach { it.close() }
}
