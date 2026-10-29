package it.unibo.tuprolog.argumentation.parallel.federation

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.EquivalenceScenarios
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse

// The scenarios on three federated sites, as on three machines: the one evaluating runs no worker, the other two
// build the graph, their messages written as text on the links
class FederatedLocalClusterTest : EquivalenceScenarios() {
    override fun distributed(
        theory: String,
        flags: Flags,
    ): (String) -> EvaluationResponse {
        val scheduler = SequentialScheduler()
        val evaluating = LocalSite(0, scheduler, flags)
        val first = LocalSite(2, scheduler)
        val second = LocalSite(1, scheduler)
        MemoryLinks.connect(evaluating.site, first.site, scheduler)
        MemoryLinks.connect(evaluating.site, second.site, scheduler)
        MemoryLinks.connect(first.site, second.site, scheduler)
        evaluating.load(theory)
        return evaluating::solve
    }
}
