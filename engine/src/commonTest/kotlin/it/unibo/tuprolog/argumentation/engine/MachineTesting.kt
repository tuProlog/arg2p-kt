package it.unibo.tuprolog.argumentation.engine

import it.unibo.tuprolog.argumentation.Arg2pPrologSolverFactory
import it.unibo.tuprolog.argumentation.engine.term.Arg2pUnificator
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

/** A solver of the machine alone for [theory], with [libraries]: unknown predicates fail, as in the solvers of arg2p */
internal fun solverWithTheory(
    theory: String = "",
    libraries: Runtime = Runtime.empty(),
) = Arg2pPrologSolverFactory.mutableSolverWithDefaultBuiltins(
    unificator = Arg2pUnificator,
    otherLibraries = libraries,
    flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL),
    staticKb = Theory.parse(theory),
)

/** Goals solved by a solver of the machine alone, one after the other */
internal class MachineTesting(
    theory: String = "",
) {
    private val solver = solverWithTheory(theory)

    /** The solutions of [goal] */
    fun solutions(goal: String): List<Solution> = solver.solve(Struct.parse(goal, solver.operators)).toList()

    /** Whether [goal] succeeds */
    fun holds(goal: String): Boolean = solver.solve(Struct.parse(goal, solver.operators)).first().isYes

    /** The values of [variable] in the solutions of [goal], as text */
    fun values(
        goal: String,
        variable: String,
    ): List<String> =
        solutions(goal)
            .filter { it.isYes }
            .map { solution -> solution.substitution.getByName(variable).toString() }

    /** The value of [variable] in the first solution of [goal], as text; null if it fails */
    fun value(
        goal: String,
        variable: String,
    ): String? =
        solver
            .solve(Struct.parse(goal, solver.operators))
            .first()
            .takeIf { it.isYes }
            ?.substitution
            ?.getByName(variable)
            ?.toString()
}
