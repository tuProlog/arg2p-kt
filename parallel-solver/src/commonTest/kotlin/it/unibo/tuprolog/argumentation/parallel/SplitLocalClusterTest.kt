package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.cluster.LocalCluster
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse

// The scenarios with the rules spread over three partitions, those supporting one another split as well
class SplitLocalClusterTest : EquivalenceScenarios() {
    override fun distributed(
        theory: String,
        flags: Flags,
    ): (String) -> EvaluationResponse = LocalCluster(parts = 3, flags = flags).also { it.load(theory) }::solve
}
