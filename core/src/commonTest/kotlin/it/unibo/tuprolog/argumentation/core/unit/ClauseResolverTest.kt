package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.engine.fsm.RenamingClauseResolver
import it.unibo.tuprolog.argumentation.core.engine.fsm.StoredClauseResolver
import it.unibo.tuprolog.argumentation.core.libs.basic.Arg2pUnificator
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Rule
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Truth
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.unify.Unificator
import kotlin.random.Random
import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * The resolution of goals with clauses as stored ([StoredClauseResolver]) against renaming them first
 * ([RenamingClauseResolver]), on random clauses and goals: same success, same variables of the goal bound, same goal
 * and body up to the names of the variables.
 */
class ClauseResolverTest {
    private fun term(
        random: Random,
        variables: kotlin.collections.List<Var>,
        depth: Int,
    ): Term =
        when (if (depth == 0) random.nextInt(3) else random.nextInt(5)) {
            0 -> variables[random.nextInt(variables.size)]
            1 -> Atom.of(listOf("a", "b")[random.nextInt(2)])
            2 -> Integer.of(random.nextInt(2))
            3 -> List.of((0 until random.nextInt(3)).map { term(random, variables, depth - 1) })
            else -> Struct.of("f", (0 until 1 + random.nextInt(2)).map { term(random, variables, depth - 1) })
        }

    // A body, with variables in the place of goals (called through call/1 once prepared)
    private fun body(
        random: Random,
        variables: kotlin.collections.List<Var>,
        depth: Int,
    ): Term =
        when (if (depth == 0) random.nextInt(3) else random.nextInt(5)) {
            0 -> variables[random.nextInt(variables.size)]
            1 -> Truth.TRUE
            2 -> Struct.of("g", term(random, variables, 2))
            3 -> Struct.of(",", body(random, variables, depth - 1), body(random, variables, depth - 1))
            else -> Struct.of(listOf(";", "->")[random.nextInt(2)], body(random, variables, depth - 1), body(random, variables, depth - 1))
        }

    private fun head(
        random: Random,
        variables: kotlin.collections.List<Var>,
    ): Struct = Struct.of("h", (0 until 3).map { term(random, variables, 2) })

    // The term with its variables replaced, in order of occurrence, by atoms
    private fun canonical(term: Term): Term {
        val variables = term.variables.distinct().toList()
        return term.apply(Substitution.of(variables.mapIndexed { i, v -> v to Atom.of("v$i") }.toMap()))
    }

    @Test
    fun storedClausesResolveAsRenamedOnes() {
        val random = Random(7)
        val reference = RenamingClauseResolver(Arg2pUnificator)
        var resolved = 0
        repeat(20000) { i ->
            val clauseVariables = (0 until 3).map { Var.of("C$it") }
            val goalVariables = (0 until 3).map { Var.of("G$it") }
            val rule = Rule.of(head(random, clauseVariables), body(random, clauseVariables + goalVariables, 2))
            val goal = head(random, goalVariables)
            // without occurs check, cyclic bindings can make the unification loop: only the resolutions that do not
            // need them are compared
            if (Unificator.default.mgu(rule.freshCopy().head, goal, true).isFailed && !goal.isGround) return@repeat
            val expected = reference.resolve(rule, goal)
            val actual = StoredClauseResolver.resolve(rule, goal)
            val case = "$i: $rule with $goal"
            assertEquals(expected != null, actual != null, case)
            if (expected == null || actual == null) return@repeat
            resolved++
            assertEquals(expected.goalBindings.keys, actual.goalBindings.keys, case)
            assertEquals(
                canonical(Struct.of("r", goal[expected.goalBindings], expected.body)),
                canonical(Struct.of("r", goal[actual.goalBindings], actual.body)),
                case,
            )
        }
        assertEquals(true, resolved > 500, "too few resolutions: $resolved")
    }
}
