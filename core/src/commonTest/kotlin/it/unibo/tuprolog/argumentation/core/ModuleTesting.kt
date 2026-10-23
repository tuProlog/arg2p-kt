package it.unibo.tuprolog.argumentation.core

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.Solution

/**
 * Goals solved by an arg2p solver for [theory], one after the other (the context of the solver carries over), to
 * test the predicates of a module: `holds("argument::claim([[r], r, [a], [], [[], [], []]], a)")`.
 */
class ModuleTesting(
    theory: String = "",
) {
    val solver = TestingUtils.solverWithTheory(theory)

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

    /** Runs [goals] in turn, each once, failing if one does not succeed */
    fun run(vararg goals: String) =
        goals.forEach { goal ->
            check(holds(goal)) { "$goal failed" }
        }
}
