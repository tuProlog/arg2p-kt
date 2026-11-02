package it.unibo.tuprolog.argumentation.parallel.cluster

import it.unibo.tuprolog.argumentation.Answer
import it.unibo.tuprolog.argumentation.LABELLINGS
import it.unibo.tuprolog.argumentation.Solution
import it.unibo.tuprolog.argumentation.checked
import it.unibo.tuprolog.argumentation.core.model.Graph
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

    /**
     * The solutions of [goal], Prolog text: the first computed at once (rejected for a goal that cannot be read), each
     * next one as it is asked for
     */
    fun solve(goal: String): Promise<AsyncSolutions> =
        replies.replied<SolutionResponse> { site.query(goal, it) }.then { first ->
            AsyncSolutions(goal, solved.checked(first), solved) { replies.replied { reply -> site.next(first.run, reply) } }
        }

    /**
     * The statements for [query], Prolog text, labelled in, out and und: one answer for each labelling, or the one across
     * them all (see the flag answerMode; rejected if the evaluation halts)
     */
    fun answer(query: String): Promise<Array<Answer>> =
        replies.replied<AnswerResponse> { site.answer(query, it) }.then { solved.answer(it).toTypedArray() }

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
        solutions
            .hasNext()
            .then { more ->
                if (!more) {
                    Promise.resolve(found)
                } else {
                    solutions
                        .nextSolution()
                        .then { solution -> graphsOf(solutions, if (checked(solution).isYes) found + solved.graph else found) }
                        .flattened()
                }
            }.flattened()
}

/**
 * The solutions of a goal (Prolog text), from the [first] reply of the site, as [solved] gives them: each next one
 * computed as it is asked for (by hasNext() or next()), none before
 */
@JsExport
class AsyncSolutions internal constructor(
    private val goal: String,
    first: SolutionResponse,
    private val solved: Solved,
    private val asked: () -> Promise<SolutionResponse>,
) {
    // the reply not given yet: the first, then the one asked for
    private var pending: Promise<SolutionResponse>? = Promise.resolve(first)

    private fun reply(): Promise<SolutionResponse> = pending ?: asked().also { pending = it }

    /** Whether the goal has another solution: computed, if not yet */
    fun hasNext(): Promise<Boolean> = reply().then { !it.exhausted }

    /** The next solution, the graph of its resolution then given by the graph() of the solver */
    fun next(): Promise<Solution> = nextSolution().then { solutionOf(it) }

    // The next solution, as the solver of the theory gives it: the reply taken at once, so that another call asks for
    // the one after it; a reply with no solution left, as it says there is no other
    internal fun nextSolution(): Promise<LogicSolution> {
        val reply = pending ?: asked()
        pending = null
        return reply.then {
            if (it.exhausted) pending = Promise.resolve(it)
            solved.given(it, goal)
        }
    }

    /** The solutions left, at most [limit] of them, each asked for once the one before it is given */
    fun toArray(limit: Int = Int.MAX_VALUE): Promise<Array<Solution>> =
        if (limit <= 0) {
            Promise.resolve(emptyArray())
        } else {
            hasNext()
                .then { more ->
                    if (!more) {
                        Promise.resolve(emptyArray())
                    } else {
                        next().then { solution -> toArray(limit - 1).then { arrayOf(solution) + it } }.flattened()
                    }
                }.flattened()
        }
}

// A promise of a promise, as JavaScript fulfils it: with the value of the inner one
private fun <T> Promise<Promise<T>>.flattened(): Promise<T> = unsafeCast<Promise<T>>()
