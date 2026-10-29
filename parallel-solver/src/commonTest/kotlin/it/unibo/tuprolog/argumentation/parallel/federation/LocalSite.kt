package it.unibo.tuprolog.argumentation.parallel.federation

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.cluster.answered
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse

/**
 * A site of the tests running [workers] workers (none by default) on [scheduler], with [flags]: its evaluations
 * answered at once, as a local cluster answers them (see LocalCluster, whose sites run as many workers as their theory
 * needs)
 */
class LocalSite(
    workers: Int? = 0,
    scheduler: Scheduler,
    flags: Flags = Flags.defaults(),
) {
    val site = Site(scheduler, workers, flags = flags)

    val stats: FederationStats get() = site.stats

    fun load(theory: String) = site.load(theory)

    fun solve(statement: String): EvaluationResponse {
        var response: EvaluationResponse? = null
        site.solve(statement) { response = it }
        return answered(response ?: error("No response for $statement"))
    }
}
