@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.core.js

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.core.model.mining.activeBranch
import it.unibo.tuprolog.argumentation.core.model.mining.graph
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.Solver
import kotlin.js.JsExport

/**
 * arg2p for JS: its solvers (see [JsSolverBuilder]), their solutions one at a time, and the argumentation graphs they
 * compute. The solvers and the solutions are tuProlog's own, read through their named methods (solveList, assertA,
 * loadStaticKb, ...), and the terms are made with [JsTerms] and [JsTheories].
 */
@JsExport
object JsArg2p {
    /** The operators of arg2p theories (rules, premises, module calls) and the default ones */
    val operators: OperatorSet by lazy { Arg2pSolver.default().operators() }

    /** An arg2p solver for [theory], with the default flags (the theory states those that differ) */
    fun solver(theory: String): MutableSolver = JsSolverBuilder().theory(theory).build()

    /** An arg2p solver for [theory], with only the flags the theory states */
    fun solverWithoutDefaults(theory: String): MutableSolver = JsSolverBuilder().theory(theory).flags("").build()

    /** The solutions of the goal written in [goal], one at a time */
    fun solve(
        solver: Solver,
        goal: String,
    ): JsSolutions = solveTerm(solver, Struct.parse(goal, solver.operators))

    /** The solutions of [goal], one at a time */
    fun solveTerm(
        solver: Solver,
        goal: Struct,
    ): JsSolutions = JsSolutions(solver.solve(goal).iterator())

    /**
     * The context branch the last resolution of [solver] ended in: the graph of its last stage. The context keeps a
     * branch for every stage of the computation (each module call made with :::), numbered from 0.
     */
    fun activeBranch(solver: Solver): Int = solver.activeBranch()

    /** The argumentation graph of the active context branch of [solver] */
    fun graph(solver: Solver): JsGraph = graphAt(solver, activeBranch(solver))

    /** The argumentation graph stored in the context branch [branch] of [solver] */
    fun graphAt(
        solver: Solver,
        branch: Int,
    ): JsGraph = toJs(solver.graph(branch))

    /**
     * The clauses stored in the context branch [branch] of [solver] matching the goal written in [goal] (facts, or
     * clause(Head, Body) for rules): the solutions of context_check(Branch, Goal), with the bindings of the variables
     * of the goal
     */
    fun contextFacts(
        solver: Solver,
        branch: Int,
        goal: String,
    ): JsSolutions = solveTerm(solver, Struct.of("context_check", Integer.of(branch), Struct.parse(goal, solver.operators)))

    private fun toJs(graph: Graph) =
        JsGraph(
            graph.labellings.map { JsArgument(it.argument.identifier, it.argument.descriptor, it.label) }.toTypedArray(),
            graph.attacks.map { JsAttack(it.attacker.identifier, it.target.identifier, it.type, it.on?.identifier) }.toTypedArray(),
        )
}
