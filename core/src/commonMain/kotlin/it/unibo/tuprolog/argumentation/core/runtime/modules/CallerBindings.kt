package it.unibo.tuprolog.argumentation.core.runtime.modules

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.SolveOptions
import it.unibo.tuprolog.solve.Solver

/**
 * Solves [goal] on behalf of a primitive, whose caller needs the bindings of all the variables of the goal.
 *
 * The solutions of a query only bind its variables with a name, as for a query typed by a user: the bindings of
 * the anonymous and wildcard ones (`_`, `_Name`), which are the caller's variables as much as the others, would
 * be lost (and with them, for instance, the bindings a module makes inside the terms it is given). They are
 * renamed for the query, and bound back in each solution.
 */
fun Solver.solveForCaller(
    goal: Struct,
    options: SolveOptions,
): Sequence<Solution> {
    val hidden =
        goal.variables
            .filter { it.isWildcard }
            .distinct()
            .associateWith { Var.of("CallerVar") }
    if (hidden.isEmpty()) return solve(goal, options)
    return solve(goal.apply(Substitution.of(hidden)).castToStruct(), options).map { solution ->
        when (solution) {
            is Solution.Yes -> Solution.yes(goal, solution.substitution.restoring(hidden))
            else -> solution
        }
    }
}

// The bindings of the renamed variables, as bindings of the variables they replaced: a renamed variable is the one it
// replaced wherever it occurs, also in the value of a binding (`same(_X, _Y)` binds `_X` to `_Y`)
private fun Substitution.Unifier.restoring(renaming: Map<Var, Var>): Substitution.Unifier {
    val inverse = Substitution.of(renaming.entries.associate { (original, renamedVar) -> renamedVar to original })
    val restored =
        renaming
            .mapValues { (_, renamedVar) -> renamedVar.apply(this).apply(inverse) }
            .filter { (original, value) -> value != original }
    val others = filterKeys { it !in renaming.values }.mapValues { (_, value) -> value.apply(inverse) }
    return Substitution.unifier(others + restored)
}
