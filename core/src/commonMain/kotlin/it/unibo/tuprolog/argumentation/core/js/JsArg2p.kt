@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.core.js

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.Solution
import kotlin.js.JsExport

/**
 * arg2p for JS: its solvers, and their solutions one at a time. The solvers and the solutions are tuProlog's own,
 * read through their named methods (solveList, assertA, loadStaticKb, ...; whenIs, valueOfByName, castToYes, ...), and
 * the terms are made with [JsTerms] and [JsTheories].
 */
@JsExport
object JsArg2p {
    /** The operators of arg2p theories (rules, premises, module calls) and the default ones */
    val operators: OperatorSet by lazy { Arg2pSolver.default().operators() }

    /** An arg2p solver for [theory], with the default flags (the theory states those that differ) */
    fun solver(theory: String): MutableSolver = Arg2pSolverFactory.default(theory = theory)

    /** An arg2p solver for [theory], with only the flags the theory states */
    fun solverWithoutDefaults(theory: String): MutableSolver =
        Arg2pSolverFactory.default(theory = theory, settings = FlagsBuilder().empty(true).create())

    /** The solutions of the goal written in [goal], one at a time */
    fun solve(
        solver: MutableSolver,
        goal: String,
    ): JsSolutions = solveTerm(solver, Struct.parse(goal, solver.operators))

    /** The solutions of [goal], one at a time */
    fun solveTerm(
        solver: MutableSolver,
        goal: Struct,
    ): JsSolutions = JsSolutions(solver.solve(goal).iterator())
}

/** The solutions of a goal, computed one at a time as they are asked for */
@JsExport
class JsSolutions internal constructor(
    private val solutions: Iterator<Solution>,
) {
    fun hasNext(): Boolean = solutions.hasNext()

    fun next(): JsSolution = JsSolution(solutions.next())
}

/** A solution of a goal: yes, no or halt (an error), with the bindings of the variables of the goal */
@JsExport
class JsSolution internal constructor(
    /** The solution of tuProlog */
    val solution: Solution,
) {
    val isYes: Boolean
        get() = solution.isYes

    val isNo: Boolean
        get() = solution.isNo

    val isHalt: Boolean
        get() = solution.isHalt

    /** The error of a halt solution, as text */
    val error: String?
        get() = solution.exception?.toString()

    /** The variables of the goal bound by a yes solution */
    val variables: Array<String>
        get() =
            if (solution.isYes) {
                solution.substitution.keys
                    .map { it.name }
                    .toTypedArray()
            } else {
                emptyArray()
            }

    /** The value of the variable of the goal named [name], if a yes solution binds it */
    fun valueOf(name: String): Term? = if (solution.isYes) solution.substitution.getByName(name) else null

    override fun toString(): String = solution.toString()
}
