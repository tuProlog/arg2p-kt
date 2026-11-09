package it.unibo.tuprolog.argumentation.core.libraries.language.parser

import it.unibo.tuprolog.argumentation.core.LibraryTesting
import it.unibo.tuprolog.argumentation.core.libraries.language.parser.helpers.RuleTerm
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull

// The terms of rules, as Prolog and Kotlin read and build them
class RuleClauseTest {
    private val module = LibraryTesting()

    @Test
    fun rules() {
        assertEquals("rl(c) :- [r, (a, b), c]", module.value("parser::ruleClause(r, (a, b), c, H, B), X = (H :- B)", "X"))
        assertEquals("rl(c) :- [f, c]", module.value("parser::premiseClause(f, c, H, B), X = (H :- B)", "X"))
        assertEquals("rl(c) :- [r, [a], c]", RuleTerm.of(Term.parse("[r, [a], c]").castToList()).toString())
        assertEquals("rl(c) :- [f, c]", RuleTerm.of(Term.parse("[f, c]").castToList()).toString())
        assertNull(RuleTerm.of(Term.parse("[f]").castToList()))
    }
}
