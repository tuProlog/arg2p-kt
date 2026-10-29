package it.unibo.tuprolog.argumentation.parallel.cluster

import it.unibo.tuprolog.argumentation.Answer
import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.core.engine.Arg2pPrologSolverFactory
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.core.model.terms.TermValues
import it.unibo.tuprolog.argumentation.parallel.message.AnswerResponse
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import it.unibo.tuprolog.argumentation.parallel.message.NO_GRAPH
import it.unibo.tuprolog.argumentation.parallel.message.STOPPED
import it.unibo.tuprolog.argumentation.parallel.message.SolutionResponse
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.exception.HaltException
import it.unibo.tuprolog.solve.exception.ResolutionException
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

    var graph: Graph = NO_GRAPH
        private set

    /** [response], unless it is an error, raised */
    fun checked(response: SolutionResponse): SolutionResponse = response.also { it.error?.let { error -> throw Arg2pException(error) } }

    /**
     * The solution of [response], a reply for [goal] (Prolog text), given: its graph the last one, what was printed
     * computing it given to [output]; a halt of the goal, if it was stopped
     */
    fun given(
        response: SolutionResponse,
        goal: String,
    ): LogicSolution {
        graph = response.graph
        if (response.stopped) return LogicSolution.halt(term(goal), HaltException(message = STOPPED, context = haltContext))
        val solution = checked(response).solution ?: throw NoSuchElementException("No more solutions")
        if (response.output.isNotEmpty()) output(response.output)
        return solution
    }

    /** The answer of [response], unless it is an error, raised */
    fun answer(response: AnswerResponse): Answer = response.answer ?: throw Arg2pException(response.error ?: "No answer")
}

// The goal of a halt, as Prolog text: as the solver reads it, or an atom if it cannot
private fun term(goal: String): Struct =
    try {
        Struct.parse(goal.trim().removeSuffix("."), TermValues.operators)
    } catch (_: Exception) {
        Atom.of(goal)
    }

// The context of the halts of the evaluations stopped, those of a resolution halted at once (made once)
private val haltContext: ExecutionContext by lazy {
    (Arg2pPrologSolverFactory.solverWithDefaultBuiltins().solveOnce(Atom.of("halt")).exception as ResolutionException).context
}
