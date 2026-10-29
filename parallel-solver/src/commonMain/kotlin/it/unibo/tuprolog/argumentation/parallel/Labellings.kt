package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.InternalArg2pApi
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.solve.Solution as LogicSolution

/**
 * The goal labelling the theory, silently, once for each labelling of its semantics: each yes solution is a labelling,
 * whose graph the solver holds while it is at the solution (for the solvers of arg2p, standard or distributed)
 */
@InternalArg2pApi
const val LABELLINGS: String = "buildLabelSetsSilent"

/**
 * The graph of each labelling, from the [solutions] of [LABELLINGS]: [graph] read at each yes solution, as it is given;
 * an Arg2pException if the evaluation halts (a theory its profile refuses)
 */
@InternalArg2pApi
fun graphsOf(
    solutions: Iterator<LogicSolution>,
    graph: () -> Graph,
): List<Graph> =
    solutions
        .asSequence()
        .mapNotNull { solution -> if (checked(solution).isYes) graph() else null }
        .toList()

/** [solution], unless it is a halt: an Arg2pException with its error then */
@InternalArg2pApi
fun checked(solution: LogicSolution): LogicSolution {
    if (solution.isHalt) {
        val error = solution.exception
        throw Arg2pException(error?.message ?: error?.toString() ?: solution.toString())
    }
    return solution
}
