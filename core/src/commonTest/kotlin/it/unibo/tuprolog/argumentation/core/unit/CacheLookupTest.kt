package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * Lookups in the cache (cache_check/1), which the arg2p engine resolves with the clauses of the cache: the answers
 * of its store (see ClauseStore.solve), newest clauses first.
 */
class CacheLookupTest {
    private val setup =
        "cache_assert(p(1)), cache_assert(p(2)), cache_assert(r(A) :- p(A)), cache_assert(t(a) :- missing)"

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
        assertEquals("[2, 1]", solve("findall(X, cache_check(p(X)), L)"))
        assertEquals("no", solve("cache_check(p(3))"))
    }

    @Test
    fun rulesAreResolvedInTheCache() {
        assertEquals("[2, 1]", solve("findall(X, cache_check(r(X)), L)"))
        assertEquals("[]", solve("findall(X, cache_check(t(X)), L)"))
    }

    @Test
    fun clauseLookupsAndConjunctions() {
        assertEquals("[p(a)]", solve("findall(B, cache_check(clause(r(a), B)), L)"))
        assertEquals("[[2, 2], [2, 1], [1, 2], [1, 1]]", solve("findall([X, Y], cache_check((p(X), p(Y))), L)"))
    }

    @Test
    fun clausesAssertedDuringALookupAreNotSeen() {
        assertEquals("[2, 1]", solve("findall(X, (cache_check(p(X)), cache_assert(p(9))), L)"))
    }
}
