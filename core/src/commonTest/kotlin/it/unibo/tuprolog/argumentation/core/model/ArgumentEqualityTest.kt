package it.unibo.tuprolog.argumentation.core.model

import it.unibo.tuprolog.core.Atom
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotEquals

/** Arguments are equal by what they are, whatever term they were read from (the argument of a compact graph has none) */
class ArgumentEqualityTest {
    private fun argument(
        conclusion: String,
        term: Boolean,
    ) = Argument(listOf("r1"), "r1", conclusion, term = if (term) Atom.of("read") else null)

    @Test
    fun withAndWithoutItsTerm() {
        assertEquals(argument("a", term = true), argument("a", term = false))
        assertEquals(argument("a", term = true).hashCode(), argument("a", term = false).hashCode())
    }

    @Test
    fun anotherConclusion() {
        assertNotEquals(argument("a", term = false), argument("b", term = false))
    }
}
