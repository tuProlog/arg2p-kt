package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.cluster.LocalCluster
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse

// The scenarios on the nodes run in turn, one partition for each group of rules supporting one another
class LocalClusterTest : EquivalenceScenarios() {
    override fun distributed(
        theory: String,
        flags: Flags,
    ): (String) -> EvaluationResponse = LocalCluster(flags = flags).also { it.load(theory) }::solve
}
