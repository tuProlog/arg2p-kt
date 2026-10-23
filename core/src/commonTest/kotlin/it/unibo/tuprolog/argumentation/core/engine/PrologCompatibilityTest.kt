package it.unibo.tuprolog.argumentation.core.engine

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
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

    // Facts are axioms and clauses strict rules, those with an identifier in a comment defeasible premises and rules:
    // they are argued as written in the language of arg2p (the flag stated in the theory is a fact, hence an axiom)
    @Test
    fun prologClausesAreArgued() {
        val solver =
            Arg2pSolverFactory.default(
                theory =
                    "prologStrictCompatibility.\nbird(tweety).\nflies(X) :- bird(X).\n" +
                        "-flies(X) :-\n    penguin(X). % r1\npenguin(tweety). % f1",
            )
        val solution = solver.solve(Struct.parse("buildLabelSets(In, Out, Und)", solver.operators)).first()
        val label = { name: String -> solution.substitution.getByName(name).toString() }
        assertEquals("[[penguin(tweety)], [flies(tweety)], [bird(tweety)], [prologStrictCompatibility]]", label("In"))
        assertEquals("[['-'(flies(tweety))]]", label("Out"))
        assertEquals("[]", label("Und"))
    }
}
