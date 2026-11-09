package it.unibo.tuprolog.argumentation.core.libraries.store

import it.unibo.tuprolog.argumentation.core.LibraryTesting
import it.unibo.tuprolog.argumentation.core.libraries.store.helpers.LabelTerm
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

// The terms of labels, as Prolog and Kotlin read and build them
class LabelTermTest {
    private val module = LibraryTesting()

    @Test
    fun labels() {
        assertEquals("out(a)", module.value("store::labelFact(out, a, X)", "X"))
        assertEquals("undId(1)", module.value("store::labelIdFact(und, 1, X)", "X"))
        assertEquals("statIn([a])", module.value("store::statementLabelFact(in, [a], X)", "X"))
        assertEquals(Term.parse("out(a)"), LabelTerm.argument("out", Atom.of("a")))
        assertEquals("undId", LabelTerm.identifierFunctor("und"))
    }
}
