package it.unibo.tuprolog.argumentation.core.libraries.framework.context

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * Lookups in the context (contextCheck/1,2), which the arg2p engine resolves with the clauses of a branch: the
 * answers of the context store (see ClauseStore.solve), newest clauses first.
 */
class ContextLookupTest {
    private val setup =
        "contextReset, contextAssert(p(1)), contextAssert(p(2)), contextAssert(q(X, X)), " +
            "contextAssert(r(A) :- p(A)), contextAssert(s(A, B) :- (p(A), p(B))), contextAssert(t(a) :- missing)"

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
        assertEquals("[2, 1]", solve("findall(X, contextCheck(p(X)), L)"))
        assertEquals("yes", solve("contextCheck(p(1)), L = yes"))
        assertEquals("no", solve("contextCheck(p(3))"))
        assertEquals("[[b, b]]", solve("findall([Y, Z], (Y = b, contextCheck(q(Y, Z))), L)"))
    }

    @Test
    fun rulesAreResolvedInTheBranch() {
        assertEquals("[2, 1]", solve("findall(X, contextCheck(r(X)), L)"))
        assertEquals("[[2, 2], [2, 1], [1, 2], [1, 1]]", solve("findall([X, Y], contextCheck(s(X, Y)), L)"))
        assertEquals("[]", solve("findall(X, contextCheck(t(X)), L)"))
    }

    @Test
    fun clauseLookups() {
        assertEquals("[p(a)]", solve("findall(B, contextCheck(clause(r(a), B)), L)"))
        assertEquals("[missing]", solve("findall(B, contextCheck(clause(t(_), B)), L)"))
        assertEquals("[]", solve("findall(B, contextCheck(clause(_, B)), L)"))
    }

    @Test
    fun conjunctions() {
        assertEquals("[[2, 2], [2, 1], [1, 2], [1, 1]]", solve("findall([X, Y], contextCheck((p(X), p(Y))), L)"))
    }

    @Test
    fun branches() {
        assertEquals(
            "[[3, 2, 1], [2, 1]]",
            solve(
                "contextActive(B0), contextBranch(B0, B1), contextAssert(p(3)), " +
                    "findall(X, contextCheck(B1, p(X)), L1), findall(X, contextCheck(B0, p(X)), L0), L = [L1, L0]",
            ),
        )
    }

    @Test
    fun clausesAssertedDuringALookupAreNotSeen() {
        assertEquals("[2, 1]", solve("findall(X, (contextCheck(p(X)), contextAssert(p(9))), L)"))
    }
}
