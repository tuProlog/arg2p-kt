package it.unibo.tuprolog.argumentation.core.libraries.store.helpers

import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

// The terms of attacks and supports, as Kotlin builds them
class GraphTermsTest {
    @Test
    fun attacks() {
        assertEquals(Term.parse("attack(rebut, a, b, c)"), AttackTerm.of(Atom.of("rebut"), Atom.of("a"), Atom.of("b"), Atom.of("c")))
    }

    @Test
    fun supports() {
        assertEquals(Term.parse("support(a, b)"), SupportTerm.of(Atom.of("a"), Atom.of("b")))
    }
}
