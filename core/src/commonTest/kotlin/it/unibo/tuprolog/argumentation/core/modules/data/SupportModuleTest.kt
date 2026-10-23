package it.unibo.tuprolog.argumentation.core.modules.data

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import it.unibo.tuprolog.argumentation.core.model.terms.SupportTerm
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

// The terms of supports, as Prolog and Kotlin read and build them
class SupportModuleTest {
    private val module = ModuleTesting()

    @Test
    fun supports() {
        assertEquals("support(a, b)", module.value("support::new(a, b, X)", "X"))
        assertTrue(module.holds("support::supporter(support(a, b), a), support::supported(support(a, b), b)"))
        assertEquals("b", SupportTerm.parts(Term.parse("support(a, b)"))!!.supported.toString())
        assertEquals(Term.parse("support(a, b)"), SupportTerm.of(Atom.of("a"), Atom.of("b")))
    }
}
