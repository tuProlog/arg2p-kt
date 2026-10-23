package it.unibo.tuprolog.argumentation.core.engine

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/**
 * Prolog clauses converted to rules (prologStrictCompatibility): the same rules each time (up to the names of their
 * variables), with ids from the positions of the clauses in the theory
 */
class PrologCompatibilityTest {
    private val theory = "prologStrictCompatibility.\np(1).\nq(X) :- p(X).\nr(X) :- q(X), \\+ s(X)."

    private fun converted(): String {
        val solver = solverWithTheory(theory)
        val solution = solver.solve(Struct.parse("context_reset, parser:::convertAllRules(R)", solver.operators)).first()
        return solution.substitution.entries
            .first { it.key.name == "R" }
            .value
            .toString()
            .replace(Regex("\\b([A-Z]\\w*?)_\\d+"), "$1")
    }

    @Test
    fun conversionsAreReproducible() {
        val rules = converted()
        assertEquals(rules, converted())
        assertTrue("rule_2" in rules && "rule_3" in rules, rules)
    }
}
