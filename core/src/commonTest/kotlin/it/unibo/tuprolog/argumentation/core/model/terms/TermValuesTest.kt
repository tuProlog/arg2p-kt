package it.unibo.tuprolog.argumentation.core.model.terms

import kotlin.test.Test
import kotlin.test.assertEquals

// The terms as arg2p writes them: a prefix negation next to its argument, the quoted atoms as they are
class TermValuesTest {
    private fun formatted(text: String) = TermValues.format(TermValues.term(text))

    @Test
    fun aNegationNextToItsArgument() {
        assertEquals("-a", formatted("-a"))
        assertEquals("f(-a, [-b])", formatted("f(- a, [- b])"))
    }

    @Test
    fun theQuotedAtomsAsTheyAre() {
        assertEquals("'a, - b'", formatted("'a, - b'"))
        assertEquals("f('(- x', -y)", formatted("f('(- x', - y)"))
    }
}
