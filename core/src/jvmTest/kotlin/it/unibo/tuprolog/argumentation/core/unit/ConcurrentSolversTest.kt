package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * Solvers resolving in different threads at the same time, each with a theory of its own (the actor solver runs
 * solvers in parallel): each finds the clauses of its own theory.
 */
class ConcurrentSolversTest {
    @Test
    fun solversInDifferentThreadsUseTheirOwnTheories() {
        val threads = 8
        val pool = Executors.newFixedThreadPool(threads)
        try {
            val results =
                (0 until threads).map { t ->
                    pool.submit<List<String>> {
                        List(30) { round ->
                            val theory = (0 until 20).joinToString("\n") { "p($t, $round, $it)." } + "\nq(X) :- p($t, $round, X)."
                            val solver = solverWithTheory(theory)
                            val solution = solver.solve(Struct.parse("findall(X, q(X), L)", solver.operators)).first()
                            val answer = solution.substitution.entries.first { it.key.name == "L" }
                            "$t/$round:${answer.value}"
                        }
                    }
                }
            val expected = (0 until 20).joinToString(", ", "[", "]")
            results.forEachIndexed { t, result ->
                result.get(5, TimeUnit.MINUTES).forEachIndexed { round, answer ->
                    assertEquals("$t/$round:$expected", answer)
                }
            }
        } finally {
            pool.shutdownNow()
        }
    }
}
