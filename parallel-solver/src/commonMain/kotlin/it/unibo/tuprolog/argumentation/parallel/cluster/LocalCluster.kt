package it.unibo.tuprolog.argumentation.parallel.cluster

import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.argumentation.parallel.federation.SequentialScheduler
import it.unibo.tuprolog.argumentation.parallel.federation.Site
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse

/**
 * A distributed solver whose nodes run in turn, on the thread of the caller: the same messages as on a cluster, in a
 * deterministic order, on every platform. The theory is split into at most [parts] partitions if given, one for each
 * group of rules supporting one another otherwise; the graph is labelled as the standard solver does with [settings].
 *
 * Local clusters sharing their [scheduler] run in turn together: federated through links delivered by it (see
 * [it.unibo.tuprolog.argumentation.parallel.federation.MemoryLinks]), they behave as sites on different machines,
 * deterministically.
 */
class LocalCluster(
    parts: Int? = null,
    settings: FlagsBuilder = FlagsBuilder(),
    val scheduler: SequentialScheduler = SequentialScheduler(),
    lends: Boolean = true,
) {
    /** The site of the cluster, to federate it with others, through its links */
    val site = Site(scheduler, parts, settings = settings, lends = lends)

    /** Replaces the theory with [theory], as Prolog text */
    fun load(theory: String) = site.load(theory)

    /** Adds [clause], as Prolog text, to the theory */
    fun add(clause: String) = site.add(clause)

    /** Forgets the theory */
    fun reset() = site.reset()

    /** The arguments for [statement], as Prolog text, by label */
    fun solve(statement: String): EvaluationResponse {
        var response: EvaluationResponse? = null
        site.solve(statement) { response = it }
        return response ?: error("No response for $statement")
    }
}
