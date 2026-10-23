package it.unibo.tuprolog.argumentation.core.modules.data

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import it.unibo.tuprolog.argumentation.core.model.terms.StatementTerm
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull

// The terms of statements, as Prolog and Kotlin read and build them
class StatementTermModuleTest {
    private val module = ModuleTesting()

    @Test
    fun statements() {
        assertEquals("[a]", module.value("statementTerm::new(a, X)", "X"))
        assertEquals(Term.parse("[a]"), StatementTerm.of(Atom.of("a")))
        assertEquals("a", StatementTerm.claim(Term.parse("[a]")).toString())
        assertNull(StatementTerm.claim(Term.parse("[a, b]")))
    }
}
