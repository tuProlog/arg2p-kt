package it.unibo.tuprolog.argumentation.parallel.consumer

import it.unibo.tuprolog.argumentation.checked
import it.unibo.tuprolog.argumentation.core.libraries.framework.utils.helpers.canonical
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.Solver

/**
 * A goal parsed once, its variables bound to terms each time it is solved: the arguments are passed as terms, never
 * written in the text of a goal (a goal carrying much data makes every step of its resolution slower, and its text
 * would be parsed each time)
 */
internal class Goal(
    text: String,
    operators: OperatorSet,
) {
    private val template = Struct.parse(text, operators)
    private val variables = template.variables.associateBy { it.name }

    /** The goal with the variables named in [bindings] bound */
    fun with(vararg bindings: Pair<String, Term>): Struct =
        template.apply(Substitution.of(bindings.associate { (name, term) -> variables.getValue(name) to term })).castToStruct()

    /**
     * The values of the variables of the goal, by name, in each solution [solver] gives it with [bindings]: those of the
     * goal alone, whatever the variables of the terms bound are named
     */
    fun solutions(
        solver: Solver,
        vararg bindings: Pair<String, Term>,
    ): List<Map<String, Term>> {
        val bound = bindings.map { it.first }.toSet()
        return solver
            .solve(with(*bindings))
            .map { checked(it) }
            .filter { it is Solution.Yes }
            .map { solution ->
                variables
                    .filterKeys { it !in bound }
                    .mapNotNull { (name, variable) -> solution.substitution[variable]?.let { name to it } }
                    .toMap()
            }.toList()
    }
}

/** The integer bound to the variable [name] */
internal fun Map<String, Term>.integer(name: String): Int = (getValue(name) as Integer).intValue.toInt()

/** The values of the variables of [goal], Prolog text, by name, in each of its solutions */
internal fun Solver.solutions(goal: String): List<Map<String, Term>> = Goal(goal, operators).solutions(this)

/** The first solution of [goal], Prolog text */
internal fun Solver.solveOnce(goal: String): Solution = solve(Struct.parse(goal, operators)).first()

/** Whether [goal], Prolog text, succeeds */
internal fun Solver.holds(goal: String): Boolean = solveOnce(goal).isYes

/** Whether [a] and [b] are the same term but for the names of their variables (as the graph store identifies them) */
internal fun sameUpToVariables(
    a: Term,
    b: Term,
): Boolean = a.canonical() == b.canonical()
