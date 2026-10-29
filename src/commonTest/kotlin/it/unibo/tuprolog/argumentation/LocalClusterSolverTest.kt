package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.argumentation.parallel.cluster.ClusterSolver
import it.unibo.tuprolog.argumentation.parallel.cluster.LocalCluster

// The scenarios on the nodes run in turn, one partition for each group of rules supporting one another
class LocalClusterSolverTest : ClusterSolverScenarios() {
    private lateinit var last: LocalCluster

    override fun cluster(
        theory: String,
        flags: Flags,
    ): ClusterSolver =
        LocalCluster(flags = flags)
            .also {
                last = it
                it.load(theory)
            }.solver

    override fun configure(flags: Flags) {
        last.flags = flags
    }
}
