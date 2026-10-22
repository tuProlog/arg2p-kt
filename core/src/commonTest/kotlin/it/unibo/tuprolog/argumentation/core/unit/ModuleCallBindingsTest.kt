package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * A module call binds all the variables of its goal for the caller, the anonymous and wildcard ones included:
 * a solution would only bind the variables with a name, as for a query typed by a user.
 */
class ModuleCallBindingsTest {
    private fun binding(
        goal: String,
        variable: String,
    ): String {
        val solver = solverWithTheory("")
        val solution = solver.solve(Struct.parse(goal, solver.operators)).first()
        return solution.substitution.entries
            .first { it.key.name == variable }
            .value
            .toString()
    }

    @Test
    fun anonymousVariableInsideABoundTerm() = assertEquals("f(1, b)", binding("S = f(_, b), utils::subtract([f(1, b)], [], [S])", "S"))

    @Test
    fun wildcardVariableInsideABoundTerm() = assertEquals("f(1, b)", binding("S = f(_W, b), utils::subtract([f(1, b)], [], [S])", "S"))

    @Test
    fun namedVariables() = assertEquals("f(1, b)", binding("utils::subtract([f(1, b)], [], [S])", "S"))
}
