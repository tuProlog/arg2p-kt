@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.parallel.cluster

import it.unibo.tuprolog.argumentation.Answer
import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.LABELLINGS
import it.unibo.tuprolog.argumentation.Solutions
import it.unibo.tuprolog.argumentation.core.model.CompactGraph
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.graphsOf
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
 * cluster and of the sites it is federated with (the whole graph, or, in query mode, the part the statement it
 * evaluates depends on, built for each), and labelled by the catalog of the cluster; the stages of a
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
    fun solve(goal: String): Solutions = solutionsOf(solutions(goal))

    /**
     * The statements for [query], Prolog text, labelled in, out and und, as Arg2p.answer gives them: one answer for each
     * labelling, or the one across them all (see answerQuery/4 and the flag answerMode)
     *
     * @throws Arg2pException if the evaluation halts (a theory its profile refuses, a query that cannot be parsed)
     */
    @JsExport.Ignore
    @JsName("answerList")
    fun answer(query: String): List<Answer> = solved.answer(replies.replied { site.answer(query, it) })

    /** The answers of [query] (see [answer]; an array in JavaScript) */
    @JsName("answer")
    fun answerArray(query: String): Array<Answer> = answer(query).toTypedArray()

    /**
     * The graph of each labelling of the theory: one, or several for a semantics with several labellings (preferred,
     * stable, ...)
     *
     * @throws Arg2pException if the evaluation halts (a theory its profile refuses)
     */
    @JsExport.Ignore
    @JsName("graphsList")
    fun graphs(): List<Graph> = graphsOf(solutions(LABELLINGS), ::graph)

    /** The graph of each labelling of the theory (an array in JavaScript) */
    @JsName("graphs")
    fun graphArray(): Array<Graph> = graphs().toTypedArray()

    /** The graph of each labelling of the theory as arrays (see [CompactGraph]): for a graph too large for [graphs] */
    @JsExport.Ignore
    @JsName("compactGraphsList")
    fun compactGraphs(): List<CompactGraph> = graphsOf(solutions(LABELLINGS)) { solved.compactGraph }

    /** The graph of each labelling of the theory as arrays (an array in JavaScript) */
    @JsName("compactGraphs")
    fun compactGraphArray(): Array<CompactGraph> = compactGraphs().toTypedArray()

    /** The graph of the last solution given as arrays (see [CompactGraph]), none before the first */
    fun compactGraph(): CompactGraph = solved.compactGraph

    /** The graph of the last solution given: the one of the last stage of its resolution (none before the first) */
    fun graph(): Graph = solved.graph

    /**
     * Stops the goal being solved, from another thread: its solution waited for is a halt, at once (`The evaluation was
     * stopped`), and the work of the cluster for it is given up; the next goals are solved as before. Nothing is done
     * if no goal is being solved.
     */
    fun stop() = site.stop()

    // The solutions of [goal], Prolog text: the first computed at once, each next one asked for as it is wanted
    private fun solutions(goal: String): Iterator<LogicSolution> {
        val first = solved.checked(replies.replied { site.query(goal, it) })
        return object : Iterator<LogicSolution> {
            // the reply not given yet: the first, then the one asked for
            private var pending: SolutionResponse? = first

            private fun reply(): SolutionResponse =
                pending ?: replies.replied<SolutionResponse> { site.next(first.run, it) }.also { pending = it }

            override fun hasNext(): Boolean = !reply().exhausted

            // a reply with no solution left, as it says there is no other
            override fun next(): LogicSolution =
                reply().let {
                    if (!it.exhausted) pending = null
                    solved.given(it, goal)
                }
        }
    }
}
