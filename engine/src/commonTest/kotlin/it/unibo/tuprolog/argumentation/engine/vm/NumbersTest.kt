package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.MachineTesting
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

// The numbers no word holds (integers beyond 28 bits, floats), compounds on the heap (see Numbers): numbers for every
// builtin, equal when their values are, and given back as they were written
class NumbersTest {
    private val machine =
        MachineTesting(
            "big(3000000000).\nbig(-3000000000).\nhuge(123456789012345678901234567890).\nhalf(0.5).\nf(2.0).\n" +
                "spin(N) :- (N =< 2999000000 -> true ; M is N - 1, spin(M)).\n",
        )

    @Test
    fun largeIntegersAreComputedAndMatched() {
        assertEquals("3000000000", machine.value("X is 1500000000 * 2", "X"))
        assertTrue(machine.holds("X is 1500000000 * 2, big(X)"))
        assertTrue(machine.holds("X is -1500000000 * 2, big(X)"))
        assertEquals(listOf("3000000000", "-3000000000"), machine.values("big(X)", "X"))
        assertEquals("123456789012345678901234567890", machine.value("huge(X)", "X"))
        assertTrue(machine.holds("huge(X), Y is X + 1, Y > X"))
        assertEquals("9223372036854775807", machine.value("X is 9223372036854775806 + 1", "X"))
        assertEquals("-9223372036854775808", machine.value("X is -9223372036854775807 - 1", "X"))
        assertTrue(machine.holds("X is 268435455 + 1, X =:= 268435456, X > 268435455"))
    }

    @Test
    fun theyAreNumbersForTheTypeTests() {
        for (n in listOf("3000000000", "123456789012345678901234567890", "0.5", "-2.5e10")) {
            assertTrue(machine.holds("X = $n, number(X), atomic(X), nonvar(X), \\+ compound(X), \\+ callable(X), \\+ atom(X)"), n)
        }
        assertTrue(machine.holds("X = 3000000000, integer(X), \\+ float(X)"))
        assertTrue(machine.holds("X = 0.5, float(X), \\+ integer(X)"))
        assertTrue(machine.holds("big(X), integer(X)"))
        assertTrue(machine.holds("half(X), float(X)"))
    }

    @Test
    fun equalValuesAreEqualTerms() {
        assertTrue(machine.holds("1.0 == 1.00"))
        assertTrue(machine.holds("X is 0.25 * 2, half(X)"))
        assertTrue(machine.holds("X = 3000000000, Y is 2999999999 + 1, X == Y"))
        assertFalse(machine.holds("1 == 1.0"))
        assertFalse(machine.holds("3000000000 = 3000000001"))
    }

    @Test
    fun theyAreOrderedByValue() {
        // (the standard order of tuProlog: floats, then integers, atoms, compounds)
        assertTrue(machine.holds("0.5 @< 2.0, 2.0 @< 1, 1 @< 3000000000, 3000000000 @< a, a @< f(1), \\+ 2.0 @< 2.00"))
        assertTrue(machine.holds("2.5 @< 3000000000, -3000000000 @< 1, 123456789012345678901234567890 @> 3000000000"))
        assertTrue(machine.holds("1.5 @< 2.5, 4000000000 @> 3000000000"))
    }

    @Test
    fun theyAreGivenBackAsWritten() {
        assertEquals("2.0", machine.value("f(X)", "X"))
        assertEquals("0.5", machine.value("half(X)", "X"))
        assertEquals("61.167", machine.value("X = 61.167", "X"))
        assertEquals("1.0E-10", machine.value("X = 1.0e-10", "X"))
        assertEquals("100.0", machine.value("X = 100.0", "X"))
    }

    @Test
    fun theyAreAtomicForFunctorArgAndUniv() {
        assertEquals("3000000000/0", machine.value("functor(3000000000, N, A), T = N/A", "T"))
        assertEquals("0.5", machine.value("functor(X, 0.5, 0)", "X"))
        assertFalse(machine.holds("catch(arg(1, 0.5, _), _, fail)"))
        assertEquals("[3000000000]", machine.value("3000000000 =.. L", "L"))
        assertEquals("f(3000000000, 0.5)", machine.value("copy_term(f(3000000000, 0.5), X)", "X"))
        assertEquals("callable", machine.value("catch(call(2.5), error(type_error(T, _), _), true)", "T"))
    }

    @Test
    fun theyAreStoredAndFoundInTheDynamicClauses() {
        assertTrue(machine.holds("assertz(price(4000000000, 1.5)), price(X, Y), X =:= 4000000000, Y =:= 1.5"))
        assertTrue(machine.holds("retract(price(4000000000, 1.5)), \\+ price(_, _)"))
        assertEquals(listOf("3000000000"), machine.values("findall(X, (big(X), X > 0), [X])", "X"))
    }

    // a million large integers made and dropped: none is kept by the program
    @Test
    fun aLoopOverLargeIntegersRuns() {
        assertTrue(machine.holds("spin(3000000000)"))
    }
}
