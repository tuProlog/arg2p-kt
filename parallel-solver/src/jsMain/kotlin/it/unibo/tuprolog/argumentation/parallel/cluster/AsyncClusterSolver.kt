package it.unibo.tuprolog.argumentation.parallel.cluster

import it.unibo.tuprolog.argumentation.Answer
import it.unibo.tuprolog.argumentation.Solution
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.parallel.LABELLINGS
import it.unibo.tuprolog.argumentation.parallel.checked
import it.unibo.tuprolog.argumentation.parallel.federation.Site
import it.unibo.tuprolog.argumentation.parallel.message.AnswerResponse
import it.unibo.tuprolog.argumentation.parallel.message.SolutionResponse
import it.unibo.tuprolog.argumentation.solutionOf
import kotlin.js.JsExport
import kotlin.js.Promise
import it.unibo.tuprolog.solve.Solution as LogicSolution

/** How a cluster of JavaScript waits for the replies of its site: [ask] asks the site, the promise fulfilled by the reply */
internal interface AsyncReplies {
    fun <T> replied(ask: ((T) -> Unit) -> Unit): Promise<T>
}

/**
 * A cluster of JavaScript as a solver of arg2p, as `Arg2p` is one, its answers promised as the workers compute them
 * (see [ClusterSolver]): any goal solved as the standard solver solves it, one solution at a time, with the graph of
 * each, and the answers of queries
 */
@JsExport
class AsyncClusterSolver internal constructor(
    private val site: Site,
    private val replies: AsyncReplies,
) {
    private val solved = Solved()

    /** Where what the solver prints goes, as the solutions are given: the console by default */
    var output: (String) -> Unit
        get() = solved.output
        set(value) {
            solved.output = value
        }

    /** The solutions of [goal], Prolog text, given one at a time as they are asked for (rejected for a goal that cannot be read) */
    fun solve(goal: String): Promise<AsyncSolutions> =
        replies.replied<SolutionResponse> { site.query(goal, it) }.then { first ->
            AsyncSolutions(Given(goal, solved.checked(first), solved)) { replies.replied { reply -> site.next(first.run, reply) } }
        }

    /** The statements for [query], Prolog text, labelled in, out and und (rejected if the evaluation halts) */
    fun answer(query: String): Promise<Answer> = replies.replied<AnswerResponse> { site.answer(query, it) }.then { solved.answer(it) }

    /** The graph of each labelling of the theory (rejected if the evaluation halts) */
    fun graphs(): Promise<Array<Graph>> = solve(LABELLINGS).then { graphsOf(it, emptyArray()) }.flattened()

    /** The graph of the last solution given: the one of the last stage of its resolution (none before the first) */
    fun graph(): Graph = solved.graph

    /**
     * Stops the goal being solved: the solution promised is a halt, at once (`The evaluation was stopped`), as the
     * other halts of a goal, and the work of the cluster for it is given up; the next goals are solved as before.
     * Nothing is done if no goal is being solved.
     */
    fun stop() = site.stop()

    // The graphs of the yes [solutions] left, after [found]
    private fun graphsOf(
        solutions: AsyncSolutions,
        found: Array<Graph>,
    ): Promise<Array<Graph>> =
        if (!solutions.hasNext()) {
            Promise.resolve(found)
        } else {
            solutions
                .nextSolution()
                .then { solution -> graphsOf(solutions, if (checked(solution).isYes) found + solved.graph else found) }
                .flattened()
        }
}

/** The solutions of a goal, given one at a time as they are asked for */
@JsExport
class AsyncSolutions internal constructor(
    private val given: Given,
    private val asked: () -> Promise<SolutionResponse>,
) {
    /** Whether the goal has another solution */
    fun hasNext(): Boolean = given.hasNext()

    /** The next solution, the graph of its resolution then given by the graph() of the solver */
    fun next(): Promise<Solution> = nextSolution().then { solutionOf(it) }

    // The next solution, as the solver of the theory gives it
    internal fun nextSolution(): Promise<LogicSolution> = (given.first()?.let { Promise.resolve(it) } ?: asked()).then { given.given(it) }

    /** The solutions left, at most [limit] of them, each asked for once the one before it is given */
    fun toArray(limit: Int = Int.MAX_VALUE): Promise<Array<Solution>> =
        if (limit <= 0 || !hasNext()) {
            Promise.resolve(emptyArray())
        } else {
            next().then { solution -> toArray(limit - 1).then { arrayOf(solution) + it } }.flattened()
        }
}

// A promise of a promise, as JavaScript fulfils it: with the value of the inner one
private fun <T> Promise<Promise<T>>.flattened(): Promise<T> = unsafeCast<Promise<T>>()
