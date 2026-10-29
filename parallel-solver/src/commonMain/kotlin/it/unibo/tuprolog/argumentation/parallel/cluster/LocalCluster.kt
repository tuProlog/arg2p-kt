@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.parallel.cluster

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.federation.FederationStats
import it.unibo.tuprolog.argumentation.parallel.federation.SequentialScheduler
import it.unibo.tuprolog.argumentation.parallel.federation.Site
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import kotlin.js.JsExport
import kotlin.jvm.JvmOverloads

/**
 * A distributed solver whose nodes run in turn, on the thread of the caller: the same messages as on a cluster, in a
 * deterministic order, on every platform. The theory is split into at most [parts] partitions if given, one for each
 * group of rules supporting one another otherwise; the graph is labelled as the standard solver does with [flags].
 *
 * Local clusters sharing their [scheduler] run in turn together: federated through links delivered by it (see
 * [it.unibo.tuprolog.argumentation.parallel.federation.MemoryLinks]), they behave as sites on different machines,
 * deterministically.
 */
@JsExport
class LocalCluster
    @JvmOverloads
    constructor(
        parts: Int? = null,
        flags: Flags = Flags.defaults(),
        @property:JsExport.Ignore val scheduler: SequentialScheduler = SequentialScheduler(),
        lends: Boolean = true,
    ) {
        /** The site of the cluster, to federate it with others, through its links */
        @JsExport.Ignore
        val site = Site(scheduler, parts, flags = flags, lends = lends)

        // The replies of the site, given at once: the nodes run in turn on this thread
        private val replies =
            object : Replies {
                override fun <T> replied(ask: ((T) -> Unit) -> Unit): T {
                    var response: T? = null
                    ask { response = it }
                    return response ?: error("No response")
                }
            }

        /**
         * The flags of the solver, the initial ones those given: those set apply from the next evaluation on, the graph
         * built anew, the links and the peers of the cluster kept
         */
        var flags: Flags
            get() = site.flags
            set(value) {
                site.flags = value
            }

        /** Replaces the theory with [theory], as Prolog text */
        fun load(theory: String) = site.load(theory)

        /** Adds [clause], as Prolog text, to the theory */
        fun add(clause: String) = site.add(clause)

        /** Forgets the theory */
        fun reset() = site.reset()

        /** The arguments for [statement], as Prolog text, by label */
        fun solve(statement: String): EvaluationResponse = answered(replies.replied { site.solve(statement, it) })

        /** The cluster as a solver of arg2p: any goal, its solutions and their graphs, the answers of queries */
        val solver: ClusterSolver = ClusterSolver(site, replies)

        /** What the cluster does: its site, and the last evaluation it answered (see [Site.stats]) */
        val stats: FederationStats get() = site.stats
    }
