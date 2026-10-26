package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.argumentation.parallel.cluster.LocalCluster
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse

// The scenarios on the nodes run in turn, one partition for each group of rules supporting one another
class LocalClusterTest : EquivalenceScenarios() {
    override fun distributed(
        theory: String,
        settings: FlagsBuilder,
    ): (String) -> EvaluationResponse = LocalCluster(settings = settings).also { it.load(theory) }::solve
}
