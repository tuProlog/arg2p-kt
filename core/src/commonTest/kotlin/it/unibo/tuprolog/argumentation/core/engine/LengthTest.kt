package it.unibo.tuprolog.argumentation.core.engine

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class LengthTest {
    private val module = ModuleTesting()

    @Test
    fun theLengthOfAList() {
        assertEquals("3", module.value("length([a, b, c], N)", "N"))
        assertEquals("0", module.value("length([], N)", "N"))
        assertTrue(module.holds("length([a, b], 2)"))
        assertFalse(module.holds("length([a, b], 3)"))
    }

    @Test
    fun aPartialListIsCompleted() {
        assertTrue(module.holds("length(L, 2), L = [_, _]"))
        assertTrue(module.holds("length([a|T], 3), T = [_, _]"))
        assertFalse(module.holds("length([a, b|_], 1)"))
        // with no length, each length in turn
        assertEquals(listOf("1", "2", "3"), module.values("length([a|_], N), (N >= 3, ! ; true)", "N"))
    }

    @Test
    fun aLengthIsANaturalNumber() {
        assertTrue(module.holds("catch(length(_, a), error(type_error(integer, a), _), true)"))
        assertTrue(module.holds("catch(length(_, -1), error(domain_error(not_less_than_zero, -1), _), true)"))
        assertTrue(module.holds("catch(length(foo, _), error(type_error(list, foo), _), true)"))
    }

    @Test
    fun modulesSeeTheBuiltin() {
        // a module solves its goals with the libraries of the solver
        assertEquals("2", module.value("utils::length([a, b], N)", "N"))
    }
}
