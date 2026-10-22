package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Real
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.unify.Unificator
import kotlin.random.Random
import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * The single-pass unification of [Arg2pUnificator] against the default unificator (without occurs check), on random
 * pairs of terms sharing variables: same success, same number of bindings, same instances.
 */
class Arg2pUnificatorTest {
    private fun term(
        random: Random,
        variables: kotlin.collections.List<Var>,
        depth: Int,
    ): Term =
        when (if (depth == 0) random.nextInt(4) else random.nextInt(7)) {
            0 -> variables[random.nextInt(variables.size)]
            1 -> Atom.of(listOf("a", "b", "c")[random.nextInt(3)])
            2 -> Integer.of(random.nextInt(3))
            3 -> if (random.nextBoolean()) Real.of(1.0) else Integer.of(1)
            4 -> List.of((0 until random.nextInt(3)).map { term(random, variables, depth - 1) })
            else -> {
                val functor = listOf("f", "g")[random.nextInt(2)]
                Struct.of(functor, (0 until 1 + random.nextInt(2)).map { term(random, variables, depth - 1) })
            }
        }

    @Test
    fun sameUnifiersAsTheDefaultUnificator() {
        val random = Random(42)
        repeat(3000) { i ->
            val variables = (0 until 4).map { Var.of("V$it") }
            val first = term(random, variables, 3)
            val second = term(random, variables, 3)
            // without occurs check, cyclic bindings can make either unificator loop: a failure with the occurs
            // check is only compared between ground terms
            if (Unificator.default.mgu(first, second, true).isFailed && !(first.isGround && second.isGround)) return@repeat
            val expected = Unificator.default.mgu(first, second, false)
            val actual = Arg2pUnificator.mgu(first, second)
            val case = "$i: $first = $second: $expected, $actual"
            assertEquals(expected.isSuccess, actual.isSuccess, case)
            assertEquals(expected.isSuccess, Arg2pUnificator.match(first, second), case)
            if (expected.isSuccess) {
                assertEquals(expected.size, actual.size, case)
                assertEquals(first.apply(expected), first.apply(actual), case)
                assertEquals(first.apply(actual), second.apply(actual), case)
            }
        }
    }
}
