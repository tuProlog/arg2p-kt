package it.unibo.tuprolog.argumentation.engine.stdlib

import it.unibo.tuprolog.argumentation.engine.MachineTesting
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class LengthTest {
    private val machine = MachineTesting()

    @Test
    fun theLengthOfAList() {
        assertEquals("3", machine.value("length([a, b, c], N)", "N"))
        assertEquals("0", machine.value("length([], N)", "N"))
        assertTrue(machine.holds("length([a, b], 2)"))
        assertFalse(machine.holds("length([a, b], 3)"))
    }

    @Test
    fun aPartialListIsCompleted() {
        assertTrue(machine.holds("length(L, 2), L = [_, _]"))
        assertTrue(machine.holds("length([a|T], 3), T = [_, _]"))
        assertFalse(machine.holds("length([a, b|_], 1)"))
        // with no length, each length in turn
        assertEquals(listOf("1", "2", "3"), machine.values("length([a|_], N), (N >= 3, ! ; true)", "N"))
    }

    @Test
    fun aLengthIsANaturalNumber() {
        assertTrue(machine.holds("catch(length(_, a), error(type_error(integer, a), _), true)"))
        assertTrue(machine.holds("catch(length(_, -1), error(domain_error(not_less_than_zero, -1), _), true)"))
        assertTrue(machine.holds("catch(length(foo, _), error(type_error(list, foo), _), true)"))
    }
}
