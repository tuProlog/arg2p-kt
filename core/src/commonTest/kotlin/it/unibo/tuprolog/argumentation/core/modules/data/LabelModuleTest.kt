package it.unibo.tuprolog.argumentation.core.modules.data

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import it.unibo.tuprolog.argumentation.core.model.terms.LabelTerm
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

// The terms of labels, as Prolog and Kotlin read and build them
class LabelModuleTest {
    private val module = ModuleTesting()

    @Test
    fun labels() {
        assertEquals(listOf("in", "out", "und"), module.values("label::label(L)", "L"))
        assertEquals("out(a)", module.value("label::argument(out, a, X)", "X"))
        assertEquals("undId(1)", module.value("label::identifier(und, 1, X)", "X"))
        assertEquals("statIn([a])", module.value("label::statement(in, [a], X)", "X"))
        assertEquals(Term.parse("out(a)"), LabelTerm.argument("out", Atom.of("a")))
        assertEquals(Term.parse("undId(a)"), LabelTerm.identifier("und", Atom.of("a")))
        assertEquals(Term.parse("statIn(a)"), LabelTerm.statement("in", Atom.of("a")))
    }
}
