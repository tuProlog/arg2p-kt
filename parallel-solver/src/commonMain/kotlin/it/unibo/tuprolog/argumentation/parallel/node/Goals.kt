package it.unibo.tuprolog.argumentation.parallel.node

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
}

/** The bindings of the variables of [goal] in each of its solutions, by name */
internal fun Solver.solutions(goal: Struct): List<Map<String, Term>> =
    solve(goal)
        .filter { it is Solution.Yes }
        .map { solution -> solution.substitution.entries.associate { (variable, value) -> variable.name to value } }
        .toList()
