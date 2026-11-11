package it.unibo.tuprolog.argumentation.core.libraries.framework.cache

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * Lookups in the cache (cacheCheck/1), which the arg2p engine resolves with the clauses of the cache: the answers
 * of its store (see ClauseStore.solve), newest clauses first.
 */
class CacheLookupTest {
    private val setup =
        "cacheAssert(p(1)), cacheAssert(p(2)), cacheAssert(r(A) :- p(A)), cacheAssert(t(a) :- missing)"

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
        assertEquals("[2, 1]", solve("findall(X, cacheCheck(p(X)), L)"))
        assertEquals("no", solve("cacheCheck(p(3))"))
    }

    @Test
    fun rulesAreResolvedInTheCache() {
        assertEquals("[2, 1]", solve("findall(X, cacheCheck(r(X)), L)"))
        assertEquals("[]", solve("findall(X, cacheCheck(t(X)), L)"))
    }

    @Test
    fun clauseLookupsAndConjunctions() {
        assertEquals("[p(a)]", solve("findall(B, cacheCheck(clause(r(a), B)), L)"))
        assertEquals("[[2, 2], [2, 1], [1, 2], [1, 1]]", solve("findall([X, Y], cacheCheck((p(X), p(Y))), L)"))
    }

    @Test
    fun clausesAssertedDuringALookupAreNotSeen() {
        assertEquals("[2, 1]", solve("findall(X, (cacheCheck(p(X)), cacheAssert(p(9))), L)"))
    }
}
