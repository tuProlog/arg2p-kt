package it.unibo.tuprolog.argumentation

import kotlin.test.Test
import kotlin.test.assertEquals

// The terms as arg2p writes them, as tuProlog writes them: a prefix operator apart from its argument, the quoted atoms as
// they are, and what is written read back as the same term
class TermValuesTest {
    private fun formatted(text: String) = TermValues.format(TermValues.term(text))

    @Test
    fun aNegationApartFromItsArgument() {
        assertEquals("- a", formatted("-a"))
        assertEquals("f(- a, [- b])", formatted("f(-a, [-b])"))
    }

    // -(-a), -(1) and the like: what is written reads back as it was (-1 would be a number)
    @Test
    fun whatIsWrittenReadsBack() {
        for (text in listOf("-holdsAt(-(-empty), 0)", "-(1)", "f(-(-1), ~(a))", "-(- a)")) {
            val written = formatted(text)
            assertEquals(TermValues.term(text), TermValues.term(written), written)
        }
    }

    @Test
    fun theQuotedAtomsAsTheyAre() {
        assertEquals("'a, - b'", formatted("'a, - b'"))
        assertEquals("f('(- x', - y)", formatted("f('(- x', -y)"))
    }
}
