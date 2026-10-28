@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.parallel.cluster

import it.unibo.tuprolog.argumentation.Answer
import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.Solutions
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.parallel.federation.Site
import it.unibo.tuprolog.argumentation.parallel.message.SolutionResponse
import it.unibo.tuprolog.argumentation.solutionsOf
import kotlin.js.JsExport
import kotlin.js.JsName
import it.unibo.tuprolog.solve.Solution as LogicSolution

/**
 * A cluster as a solver of arg2p, as `Arg2p` is one, on the theory and the flags of the cluster: any goal solved as the
 * standard solver solves it (buildLabelSets/3, answerQuery/4, the Prolog predicates of the theory, ...), one solution
 * at a time, with the graph of each, and the answers of queries. The graph a goal needs is built by the workers of the
 * cluster and of the sites it is federated with (the whole graph, or, in query mode, the part the statements it
 * evaluates depend on, grown by the later ones), and labelled by the catalog of the cluster; the stages of a
 * resolution are not kept, the graph of the last one alone.
 */
@JsExport
class ClusterSolver internal constructor(
    private val site: Site,
    private val replies: Replies,
) {
    private val solved = Solved()

    /** Where what the solver prints goes, as the solutions are given: the standard output by default */
    var output: (String) -> Unit
        get() = solved.output
        set(value) {
            solved.output = value
        }

    /**
     * The solutions of [goal], Prolog text, given one at a time as they are asked for (the first one computed at once)
     *
     * @throws Arg2pException for a goal that cannot be read
     */
    fun solve(goal: String): Solutions = solutionsOf(Given(solved.checked(replies.replied { site.query(goal, it) })))

    /**
     * The statements for [query], Prolog text, labelled in, out and und (see answerQuery/4)
     *
     * @throws Arg2pException if the evaluation halts (a theory its profile refuses, a query that cannot be parsed)
     */
    fun answer(query: String): Answer = solved.answer(replies.replied { site.answer(query, it) })

    /**
     * The graph of each labelling of the theory: one, or several for a semantics with several labellings (preferred,
     * stable, ...)
     *
     * @throws Arg2pException if the evaluation halts (a theory its profile refuses)
     */
    @JsExport.Ignore
    @JsName("graphsList")
    fun graphs(): List<Graph> =
        solve("buildLabelSetsSilent")
            .iterator()
            .asSequence()
            .mapNotNull { solution ->
                if (solution.isHalt) throw Arg2pException(solution.error ?: solution.toString())
                if (solution.isYes) graph() else null
            }.toList()

    /** The graph of each labelling of the theory (an array in JavaScript) */
    @JsName("graphs")
    fun graphArray(): Array<Graph> = graphs().toTypedArray()

    /** The graph of the last solution given: the one of the last stage of its resolution (none before the first) */
    fun graph(): Graph = solved.graph

    // The solutions of a goal, from the [first] reply of the site
    private inner class Given(
        first: SolutionResponse,
    ) : Iterator<LogicSolution> {
        private val run = first.run
        private var pending: SolutionResponse? = first
        private var more = true

        override fun hasNext(): Boolean = pending != null || more

        override fun next(): LogicSolution {
            val response = pending ?: replies.replied { site.next(run, it) }
            pending = null
            more = response.hasNext
            return solved.given(response)
        }
    }
}
