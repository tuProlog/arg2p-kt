package it.unibo.tuprolog.argumentation.core.libs.data

import it.unibo.tuprolog.argumentation.core.mining.labels
import it.unibo.tuprolog.argumentation.core.mining.supports

import it.unibo.tuprolog.argumentation.core.modules.data.AttackTerm
import it.unibo.tuprolog.argumentation.core.modules.data.LabelTerm
import it.unibo.tuprolog.argumentation.core.modules.data.RuleTerm
import it.unibo.tuprolog.argumentation.core.modules.data.StatementTerm
import it.unibo.tuprolog.argumentation.core.modules.data.SupportTerm

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull
import kotlin.test.assertTrue

// The terms of attacks, supports, labels, statements and rules, as Prolog and Kotlin read and build them
class DataTermsTest {
    private val module = ModuleTesting()

    @Test
    fun attacks() {
        assertEquals("attack(rebut, a, b, c)", module.value("attack::new(rebut, a, b, c, X)", "X"))
        val attack = "attack(rebut, a, b, c)"
        assertTrue(module.holds("attack::type($attack, rebut), attack::attacker($attack, a)"))
        assertTrue(module.holds("attack::attacked($attack, b), attack::on($attack, c)"))
        val parts = AttackTerm.parts(Term.parse("attack(rebut, a, b, c)"))!!
        assertEquals(listOf("rebut", "a", "b", "c"), listOf(parts.type, parts.attacker, parts.attacked, parts.on).map { it.toString() })
        assertEquals(Term.parse("attack(rebut, a, b, c)"), AttackTerm.of(Atom.of("rebut"), Atom.of("a"), Atom.of("b"), Atom.of("c")))
        assertNull(AttackTerm.parts(Term.parse("support(a, b)")))
    }

    @Test
    fun supports() {
        assertEquals("support(a, b)", module.value("support::new(a, b, X)", "X"))
        assertTrue(module.holds("support::supporter(support(a, b), a), support::supported(support(a, b), b)"))
        assertEquals("b", SupportTerm.parts(Term.parse("support(a, b)"))!!.supported.toString())
        assertEquals(Term.parse("support(a, b)"), SupportTerm.of(Atom.of("a"), Atom.of("b")))
    }

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

    @Test
    fun statements() {
        assertEquals("[a]", module.value("statementTerm::new(a, X)", "X"))
        assertEquals(Term.parse("[a]"), StatementTerm.of(Atom.of("a")))
        assertEquals("a", StatementTerm.claim(Term.parse("[a]")).toString())
        assertNull(StatementTerm.claim(Term.parse("[a, b]")))
    }

    @Test
    fun rules() {
        assertEquals("rl(c) :- [r, (a, b), c]", module.value("rule::rule(r, (a, b), c, H, B), X = (H :- B)", "X"))
        assertEquals("rl(c) :- [f, c]", module.value("rule::premise(f, c, H, B), X = (H :- B)", "X"))
        assertEquals("rl(c) :- [r, [a], c]", RuleTerm.rule(Atom.of("r"), Term.parse("[a]"), Atom.of("c")).toString())
        assertEquals("rl(c) :- [f, c]", RuleTerm.of(Term.parse("[f, c]").castToList()).toString())
        assertNull(RuleTerm.of(Term.parse("[f]").castToList()))
    }
}
