package it.unibo.tuprolog.argumentation.parallel.cluster

import it.unibo.tuprolog.argumentation.Answer
import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.parallel.message.AnswerResponse
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import it.unibo.tuprolog.argumentation.parallel.message.SolutionResponse
import it.unibo.tuprolog.solve.Solution as LogicSolution

/** [response], unless it is an error, raised */
internal fun answered(response: EvaluationResponse): EvaluationResponse = response.error?.let { throw Arg2pException(it) } ?: response

/** How a cluster waits for the replies of its site: [ask] asks the site, the reply returned once it gives it */
internal interface Replies {
    fun <T> replied(ask: ((T) -> Unit) -> Unit): T
}

/**
 * The replies of a site to the solver of a cluster, as the solver gives them: the solution of each (an error raised),
 * the graph of the last one, and what the solver printed computing them, given to [output]
 */
internal class Solved {
    var output: (String) -> Unit = { print(it) }

    var graph: Graph = Graph(emptyList(), emptyList(), emptyList())
        private set

    /** [response], unless it is an error, raised */
    fun checked(response: SolutionResponse): SolutionResponse = response.also { it.error?.let { error -> throw Arg2pException(error) } }

    /** The solution of [response], given: its graph the last one, what was printed computing it given to [output] */
    fun given(response: SolutionResponse): LogicSolution {
        val solution = checked(response).solution ?: throw NoSuchElementException("No more solutions")
        graph = response.graph
        if (response.output.isNotEmpty()) output(response.output)
        return solution
    }

    /** The answer of [response], unless it is an error, raised */
    fun answer(response: AnswerResponse): Answer = response.answer ?: throw Arg2pException(response.error ?: "No answer")
}
