package it.unibo.tuprolog.argumentation.parallel.federation

import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.argumentation.parallel.EquivalenceScenarios
import it.unibo.tuprolog.argumentation.parallel.cluster.LocalCluster
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse

// The scenarios on three sites linked as a star: the two running the workers have no link between them, their
// messages go through the evaluating one
class RelayedLocalClusterTest : EquivalenceScenarios() {
    override fun distributed(
        theory: String,
        settings: FlagsBuilder,
    ): (String) -> EvaluationResponse {
        val scheduler = SequentialScheduler()
        val evaluating = LocalCluster(parts = 0, settings = settings, scheduler = scheduler)
        val first = LocalCluster(parts = 2, scheduler = scheduler)
        val second = LocalCluster(parts = 1, scheduler = scheduler)
        MemoryLinks.connect(evaluating.site, first.site, scheduler)
        MemoryLinks.connect(evaluating.site, second.site, scheduler)
        evaluating.load(theory)
        return evaluating::solve
    }
}
