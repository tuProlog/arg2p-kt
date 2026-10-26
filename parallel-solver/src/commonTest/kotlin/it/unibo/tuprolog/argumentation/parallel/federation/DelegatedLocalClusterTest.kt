package it.unibo.tuprolog.argumentation.parallel.federation

import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.argumentation.parallel.EquivalenceScenarios
import it.unibo.tuprolog.argumentation.parallel.cluster.LocalCluster
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse

// The scenarios on a site lending the workers of its delegates (as the threads of a JS runtime) to the evaluating
// one, linked to it alone: every message of a worker goes through the site of the delegates
class DelegatedLocalClusterTest : EquivalenceScenarios() {
    override fun distributed(
        theory: String,
        settings: FlagsBuilder,
    ): (String) -> EvaluationResponse {
        val scheduler = SequentialScheduler()
        val evaluating = LocalCluster(parts = 0, settings = settings, scheduler = scheduler)
        val lending = LocalCluster(parts = 0, scheduler = scheduler)
        repeat(2) { MemoryLinks.delegate(lending.site, LocalCluster(parts = 1, scheduler = scheduler).site, scheduler) }
        MemoryLinks.connect(evaluating.site, lending.site, scheduler)
        evaluating.load(theory)
        return evaluating::solve
    }
}
