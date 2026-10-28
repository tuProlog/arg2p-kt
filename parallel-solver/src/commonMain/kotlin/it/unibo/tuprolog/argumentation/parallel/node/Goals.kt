package it.unibo.tuprolog.argumentation.parallel.node

import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.core.model.terms.ArgumentTerm
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.unify.Unificator

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
            .filter { it is Solution.Yes }
            .map { solution ->
                variables
                    .filterKeys { it !in bound }
                    .mapNotNull { (name, variable) -> solution.substitution[variable]?.let { name to it } }
                    .toMap()
            }.toList()
    }
}

/** The values of the variables of [goal], Prolog text, by name, in each of its solutions */
internal fun Solver.solutions(goal: String): List<Map<String, Term>> = Goal(goal, operators).solutions(this)

/** The first solution of [goal], Prolog text */
internal fun Solver.solveOnce(goal: String): Solution = solve(Struct.parse(goal, operators)).first()

/** Whether [goal], Prolog text, succeeds */
internal fun Solver.holds(goal: String): Boolean = solveOnce(goal).isYes

/** [solution], unless it is a halt: its error raised, as an [Arg2pException] */
internal fun checked(solution: Solution): Solution {
    if (solution.isHalt) {
        val error = solution.exception
        throw Arg2pException(error?.message ?: error?.toString() ?: solution.toString())
    }
    return solution
}

/** Whether exploring [general] explores [specific] as well: [specific], its variables fixed, is an instance of it */
internal fun covers(
    general: Term,
    specific: Term,
): Boolean {
    val fixed = specific.variables.distinct().mapIndexed { i, variable -> variable to Atom.of("fixed variable $i") }
    return Unificator.default.match(general, specific.apply(Substitution.of(fixed.toMap())))
}

/** The statement [argument], the term of an argument, concludes */
internal fun claimOf(argument: Term): Term = ArgumentTerm.parts(argument)!!.claim!!
