package it.unibo.tuprolog.argumentation.core.modules.data

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import it.unibo.tuprolog.argumentation.core.model.terms.RuleTerm
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull

// The terms of rules, as Prolog and Kotlin read and build them
class RuleModuleTest {
    private val module = ModuleTesting()

    @Test
    fun rules() {
        assertEquals("rl(c) :- [r, (a, b), c]", module.value("rule::rule(r, (a, b), c, H, B), X = (H :- B)", "X"))
        assertEquals("rl(c) :- [f, c]", module.value("rule::premise(f, c, H, B), X = (H :- B)", "X"))
        assertEquals("rl(c) :- [r, [a], c]", RuleTerm.rule(Atom.of("r"), Term.parse("[a]"), Atom.of("c")).toString())
        assertEquals("rl(c) :- [f, c]", RuleTerm.of(Term.parse("[f, c]").castToList()).toString())
        assertNull(RuleTerm.of(Term.parse("[f]").castToList()))
    }
}
