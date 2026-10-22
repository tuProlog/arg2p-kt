package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * Lookups in the context (context_check/1,2), which the arg2p engine resolves with the clauses of a branch: the
 * answers of the context store (see ClauseStore.solve), newest clauses first.
 */
class ContextLookupTest {
    private val setup =
        "context_reset, context_assert(p(1)), context_assert(p(2)), context_assert(q(X, X)), " +
            "context_assert(r(A) :- p(A)), context_assert(s(A, B) :- (p(A), p(B))), context_assert(t(a) :- missing)"

    private fun solve(goal: String): String {
        val solver = solverWithTheory("")
        val solution = solver.solve(Struct.parse("$setup, $goal", solver.operators)).first()
        return if (solution.isYes) {
            solution.substitution.entries
                .filter { it.key.name == "L" }
                .joinToString { it.value.toString() }
        } else {
            "no"
        }
    }

    @Test
    fun facts() {
        assertEquals("[2, 1]", solve("findall(X, context_check(p(X)), L)"))
        assertEquals("yes", solve("context_check(p(1)), L = yes"))
        assertEquals("no", solve("context_check(p(3))"))
        assertEquals("[[b, b]]", solve("findall([Y, Z], (Y = b, context_check(q(Y, Z))), L)"))
    }

    @Test
    fun rulesAreResolvedInTheBranch() {
        assertEquals("[2, 1]", solve("findall(X, context_check(r(X)), L)"))
        assertEquals("[[2, 2], [2, 1], [1, 2], [1, 1]]", solve("findall([X, Y], context_check(s(X, Y)), L)"))
        assertEquals("[]", solve("findall(X, context_check(t(X)), L)"))
    }

    @Test
    fun clauseLookups() {
        assertEquals("[p(a)]", solve("findall(B, context_check(clause(r(a), B)), L)"))
        assertEquals("[missing]", solve("findall(B, context_check(clause(t(_), B)), L)"))
        assertEquals("[]", solve("findall(B, context_check(clause(_, B)), L)"))
    }

    @Test
    fun conjunctions() {
        assertEquals("[[2, 2], [2, 1], [1, 2], [1, 1]]", solve("findall([X, Y], context_check((p(X), p(Y))), L)"))
    }

    @Test
    fun branches() {
        assertEquals(
            "[[3, 2, 1], [2, 1]]",
            solve(
                "context_active(B0), context_branch(B0, B1), context_assert(p(3)), " +
                    "findall(X, context_check(B1, p(X)), L1), findall(X, context_check(B0, p(X)), L0), L = [L1, L0]",
            ),
        )
    }

    @Test
    fun clausesAssertedDuringALookupAreNotSeen() {
        assertEquals("[2, 1]", solve("findall(X, (context_check(p(X)), context_assert(p(9))), L)"))
    }
}
