package it.unibo.tuprolog.argumentation.parallel.federation

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.EquivalenceScenarios
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse

// The scenarios on a site lending the workers of its delegates (as the threads of a JS runtime) to the evaluating
// one, linked to it alone: every message of a worker goes through the site of the delegates
class DelegatedLocalClusterTest : EquivalenceScenarios() {
    override fun distributed(
        theory: String,
        flags: Flags,
    ): (String) -> EvaluationResponse {
        val scheduler = SequentialScheduler()
        val evaluating = LocalSite(0, scheduler, flags)
        val lending = LocalSite(0, scheduler)
        repeat(2) { MemoryLinks.delegate(lending.site, LocalSite(1, scheduler).site, scheduler) }
        MemoryLinks.connect(evaluating.site, lending.site, scheduler)
        evaluating.load(theory)
        return evaluating::solve
    }
}
