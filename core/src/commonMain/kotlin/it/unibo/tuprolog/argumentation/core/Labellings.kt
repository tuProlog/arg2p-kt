package it.unibo.tuprolog.argumentation.core

import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.SyntaxProblem
import it.unibo.tuprolog.argumentation.core.model.terms.TermValues
import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.operators.OperatorSet
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
fun <G : Any> graphsOf(
    solutions: Iterator<LogicSolution>,
    graph: () -> G,
): List<G> =
    solutions
        .asSequence()
        .mapNotNull { solution -> if (checked(solution).isYes) graph() else null }
        .toList()

/** [solution], unless it is a halt: an Arg2pException with its error then */
@InternalArg2pApi
fun checked(solution: LogicSolution): LogicSolution {
    if (solution.isHalt) throw Arg2pException(solution.haltMessage())
    return solution
}

/** The error of [this], a halt, as text */
@InternalArg2pApi
fun LogicSolution.haltMessage(): String = exception?.let { it.message ?: it.toString() } ?: toString()

/** The term [text] reads as, with [operators]: an Arg2pException telling where it does not parse (as a query) */
@InternalArg2pApi
fun termOf(
    text: String,
    operators: OperatorSet,
): Term = SyntaxProblem.parsing("Query") { TermValues.term(text, operators) }

/** The goal [text] reads as, with [operators]: an Arg2pException if it does not parse, or is no goal */
@InternalArg2pApi
fun goalOf(
    text: String,
    operators: OperatorSet,
): Struct = termOf(text, operators).let { it as? Struct ?: throw Arg2pException("Not a goal: $text") }
