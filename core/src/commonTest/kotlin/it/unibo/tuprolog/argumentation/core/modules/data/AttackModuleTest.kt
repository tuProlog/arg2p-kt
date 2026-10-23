package it.unibo.tuprolog.argumentation.core.modules.data

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import it.unibo.tuprolog.argumentation.core.model.terms.AttackTerm
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull
import kotlin.test.assertTrue

// The terms of attacks, as Prolog and Kotlin read and build them
class AttackModuleTest {
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
}
