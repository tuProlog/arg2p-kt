package it.unibo.tuprolog.argumentation.lsp

import it.unibo.tuprolog.argumentation.Arg2p
import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.Solutions
import it.unibo.tuprolog.argumentation.parallel.cluster.ThreadedCluster

/**
 * The resolution of a query of a page, whatever solves it: the solver of the page (see [Local]) or the cluster of the
 * federation (see [Federated]). The engine of the pages asks it one request at a time.
 */
internal interface Resolution {
    /**
     * The next solution, with the graph of its stage, and whether there may be another one
     *
     * @throws Exception if the solution cannot be computed
     */
    fun next(): Pair<SolutionData, Boolean>

    /** The graph of the stage [branch] of the resolution: none for a stage that cannot be read */
    fun graphAt(branch: Int): GraphData

    /** What resolves the queries the pages run in parallel: the federation of the language server (see [Federated]) */
    fun interface Factory {
        /**
         * The resolution of [query] on [theory] with [flags], of [profile], what it prints going to [output]: its first
         * solution computed at once. A CancellationException ([STOPPED]) if the thread asking is interrupted.
         */
        fun resolve(
            theory: String,
            flags: Flags,
            query: String,
            profile: String,
            output: (String) -> Unit,
        ): Resolution
    }

    /** The resolution of [solutions] of [solver], whose theory is of [profile]: every stage of it can be read back */
    class Local(
        private val solver: Arg2p,
        private val profile: String,
        private val solutions: Solutions,
    ) : Resolution {
        override fun next(): Pair<SolutionData, Boolean> {
            val found = solutions.next()
            val (graph, branch) =
                try {
                    solver.activeBranch.let { GraphData.of(solver.graphAt(it), profile) to it }
                } catch (_: Exception) {
                    // no graph: a plain Prolog goal builds none
                    GraphData.EMPTY to 0
                }
            return SolutionData.of(found, graph, branch) to solutions.hasNext()
        }

        override fun graphAt(branch: Int): GraphData =
            try {
                GraphData.of(solver.graphAt(branch), profile)
            } catch (_: Exception) {
                GraphData.EMPTY
            }
    }

    /**
     * The resolution of [query] on [theory] with [flags] by the cluster of [federation], of [profile], what it prints
     * going to [output]: its first solution computed at once (see [Factory.resolve]). The cluster keeps the graph of the
     * last stage alone: it is the graph of every stage.
     */
    class Federated(
        private val federation: Federation,
        private val theory: String,
        private val flags: Flags,
        query: String,
        private val profile: String,
        private val output: (String) -> Unit,
    ) : Resolution {
        // the cluster solving it: the solutions are no longer computed once the federation made another one
        private lateinit var owner: ThreadedCluster
        private val solutions =
            federation.request(theory, flags, output) {
                owner = it
                it.solver.solve(query)
            }

        // the graph of the last solution, the only stage the cluster keeps
        @Volatile private var last = GraphData.EMPTY

        override fun next(): Pair<SolutionData, Boolean> =
            federation.request(theory, flags, output) { running ->
                check(running === owner) { NO_LONGER_RUNNING }
                val found = solutions.next()
                last = GraphData.of(running.solver.graph(), profile)
                SolutionData.of(found, last, 0) to solutions.hasNext()
            }

        override fun graphAt(branch: Int): GraphData = last
    }

    companion object {
        /** What a request of a query no longer running fails with */
        const val NO_LONGER_RUNNING = "This query is no longer running."

        /** What a request that was stopped fails with */
        const val STOPPED = "The query was stopped."
    }
}
