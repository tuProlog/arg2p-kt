package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.engine.fsm.FirstArgumentIndex
import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Fact
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Real
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import kotlin.random.Random
import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * The clauses [FirstArgumentIndex] gives for a goal, once matched, against matching all the clauses of the predicate:
 * the same clauses, in the same order, on random predicates and goals.
 */
class FirstArgumentIndexTest {
    private fun argument(
        random: Random,
        depth: Int,
    ): Term =
        when (if (depth == 0) random.nextInt(5) else random.nextInt(7)) {
            0 -> Var.of("X")
            1 -> Atom.of(listOf("a", "b", "[]")[random.nextInt(3)])
            2 -> Integer.of(random.nextInt(3))
            3 -> Real.of(1.0)
            4 -> Integer.of(1)
            5 -> List.of((0 until 1 + random.nextInt(2)).map { argument(random, depth - 1) })
            else -> Struct.of(listOf("f", "g")[random.nextInt(2)], (0 until 1 + random.nextInt(2)).map { argument(random, depth - 1) })
        }

    private fun head(random: Random): Struct = Struct.of("p", argument(random, 2), argument(random, 1))

    @Test
    fun sameClausesAsMatchingThemAll() {
        val random = Random(11)
        repeat(300) { i ->
            val clauses: kotlin.collections.List<Clause> = (0 until random.nextInt(1, 30)).map { Fact.of(head(random)) }
            val index = FirstArgumentIndex(clauses)
            repeat(20) {
                val goal = head(random)
                val expected = clauses.filter { Arg2pUnificator.match(goal, it.head!!) }
                val actual = index.candidates(goal).filter { Arg2pUnificator.match(goal, it.head!!) }
                assertEquals(expected, actual, "$i: $goal in $clauses")
            }
        }
    }
}
