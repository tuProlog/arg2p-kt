package it.unibo.tuprolog.argumentation.lsp

import it.unibo.tuprolog.argumentation.Arg2p
import it.unibo.tuprolog.argumentation.Solutions

/**
 * The resolution of a query of a page, whatever solves it: the solver of the page (see [Local]) or the cluster of the
 * federation (see [Federation.resolve]). The engine of the pages asks it one request at a time.
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
}
