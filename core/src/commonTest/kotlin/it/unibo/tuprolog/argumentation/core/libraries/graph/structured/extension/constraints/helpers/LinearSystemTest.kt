package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.constraints.helpers

import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import kotlin.random.Random
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

/**
 * The simplex on exact rationals decides whether linear constraints hold together over dense numbers: cases on the
 * boundaries worked out by hand, and random systems against Fourier-Motzkin elimination (an independent exact procedure,
 * written here as the oracle only)
 */
class LinearSystemTest {
    // (parsed as one term, for a variable to be the same in all of them)
    private fun satisfiable(vararg constraints: String): Boolean =
        LinearSystem.satisfiable(LinearTerms.rows(Term.parse("[${constraints.joinToString(", ")}]").castToList().toList()))

    @Test
    fun boundsMeetOnlyWhereOneIsClosed() {
        assertTrue(satisfiable("T >= 7", "T =< 7"))
        assertFalse(satisfiable("T > 7", "T =< 7"))
        assertFalse(satisfiable("T > 7", "T = 7"))
        assertTrue(satisfiable("T > 7", "T < 7.001"))
        assertFalse(satisfiable("T > 1", "T < 1"))
    }

    @Test
    fun numbersAreDense() {
        assertTrue(satisfiable("X > 2", "X < 3"))
        assertTrue(satisfiable("X < Y", "Y < X + 1"))
        assertFalse(satisfiable("X < Y", "Y < X"))
        assertFalse(satisfiable("X =< Y - 1", "Y =< X"))
    }

    @Test
    fun linksAndCoefficients() {
        assertTrue(satisfiable("2 * X + 3 * Y =< 6", "X >= 3", "Y >= 0"))
        assertFalse(satisfiable("2 * X + 3 * Y =< 6", "X >= 3", "Y > 0"))
        assertTrue(satisfiable("X / 2 =:= Y", "Y = 3", "X > 5.5"))
        assertFalse(satisfiable("T > X", "X > 6", "X < 8", "T =< 6"))
        assertTrue(satisfiable("T > X", "X > 6", "X < 8", "T =< 6.5"))
    }

    @Test
    fun groundConstraintsAreChecked() {
        assertTrue(satisfiable("3 < 4", "T > 0"))
        assertFalse(satisfiable("4 < 3"))
        assertTrue(satisfiable())
    }

    @Test
    fun onlyLinearComparisonsAreDecided() {
        assertTrue(LinearTerms.isLinear(Term.parse("2 * X - Y / 4 >= -3")))
        assertFalse(LinearTerms.isLinear(Term.parse("X * Y > 3")))
        assertFalse(LinearTerms.isLinear(Term.parse("X = a")))
        assertFalse(LinearTerms.isLinear(Term.parse("member(X, [1, 2])")))
        assertFalse(LinearTerms.isLinear(Term.parse("X =\\= 3")))
    }

    @Test
    fun randomSystemsAgreeWithElimination() {
        val random = Random(20261109)
        repeat(3000) {
            val variables = 1 + random.nextInt(3)
            val rows =
                List(1 + random.nextInt(5)) {
                    val coefficients =
                        (0 until variables)
                            .associateWith { Rational.of(random.nextLong(-3, 4)) }
                            .filterValues { !it.isZero }
                    LinearRow(coefficients, Rational.of(random.nextLong(-5, 6)), LinearRow.Kind.entries[random.nextInt(3)])
                }
            assertEquals(eliminated(rows), LinearSystem.satisfiable(rows), "$rows")
        }
    }

    // Fourier-Motzkin: each variable eliminated by pairing its lower and upper bounds; what is left has no variable
    private fun eliminated(system: List<LinearRow>): Boolean {
        var rows =
            system.flatMap { row ->
                if (row.kind == LinearRow.Kind.EQUAL) {
                    listOf(
                        LinearRow(row.coefficients, row.constant, LinearRow.Kind.AT_MOST),
                        LinearRow(row.coefficients.mapValues { -it.value }, -row.constant, LinearRow.Kind.AT_MOST),
                    )
                } else {
                    listOf(row)
                }
            }
        val variables = system.flatMap { it.coefficients.keys }.toSet()
        for (variable in variables) {
            val (with, without) = rows.partition { !(it.coefficients[variable] ?: Rational.ZERO).isZero }
            val upper = with.filter { it.coefficients.getValue(variable).signum > 0 }
            val lower = with.filter { it.coefficients.getValue(variable).signum < 0 }
            rows = without +
                lower.flatMap { l ->
                    upper.map { u ->
                        val a = -l.coefficients.getValue(variable)
                        val b = u.coefficients.getValue(variable)
                        val keys = (l.coefficients.keys + u.coefficients.keys) - variable
                        val coefficients =
                            keys.associateWith { k ->
                                (l.coefficients[k] ?: Rational.ZERO) * b + (u.coefficients[k] ?: Rational.ZERO) * a
                            }
                        val strict = l.kind == LinearRow.Kind.BELOW || u.kind == LinearRow.Kind.BELOW
                        LinearRow(
                            coefficients,
                            l.constant * b + u.constant * a,
                            if (strict) LinearRow.Kind.BELOW else LinearRow.Kind.AT_MOST,
                        )
                    }
                }
        }
        return rows.all { if (it.kind == LinearRow.Kind.BELOW) Rational.ZERO < it.constant else Rational.ZERO <= it.constant }
    }
}
