package it.unibo.tuprolog.argumentation.core.libraries.arg2p.helpers

import it.unibo.tuprolog.argumentation.checked
import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi
import it.unibo.tuprolog.solve.Solution as LogicSolution

/**
 * The goal labelling the theory, silently, once for each labelling of its semantics: each yes solution is a labelling,
 * whose graph the solver holds while it is at the solution (for the solvers of arg2p, standard or distributed)
 */
@InternalArg2pApi
const val LABELLINGS: String = "solve"

/**
 * The graph of each labelling, from the [solutions] of [LABELLINGS]: [graph] read at each yes solution, as it is given;
 * an Arg2pException if the evaluation halts (a theory its profile refuses)
 */
@InternalArg2pApi
fun <G : Any> graphsOf(
    solutions: Iterator<LogicSolution>,
    graph: () -> G,
): List<G> =
    solutions
        .asSequence()
        .mapNotNull { solution -> if (checked(solution).isYes) graph() else null }
        .toList()
