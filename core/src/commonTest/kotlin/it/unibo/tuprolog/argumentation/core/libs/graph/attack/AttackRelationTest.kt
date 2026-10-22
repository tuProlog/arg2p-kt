package it.unibo.tuprolog.argumentation.core.libs.graph.attack

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class AttackRelationTest {
    private val module = ModuleTesting()

    // Arguments of one defeasible rule (r), one premise (f) or one strict rule (s), with a conclusion and a body
    private fun rule(
        id: String,
        claim: String,
        body: String = "[]",
    ) = "[[$id], $id, [$claim], $body, [[$id], [$id], []]]"

    private fun premise(
        id: String,
        claim: String,
    ) = "[[$id], none, [$claim], [], [[], [], [$id]]]"

    private fun strict(
        id: String,
        claim: String,
    ) = "[[$id], $id, [$claim], [], [[], [], []]]"

    private fun attack(
        attacker: String,
        attacked: String,
    ): String? = module.value("attackRelation::attacks(T, $attacker, $attacked)", "T")

    @Test
    fun rebutOnConflictingConclusions() {
        assertEquals("rebut", attack(rule("r1", "a"), rule("r2", "-a")))
        assertEquals("rebut", attack(strict("s1", "a"), rule("r2", "-a")))
        // a strict argument is not rebutted
        assertEquals(null, attack(rule("r1", "a"), strict("s2", "-a")))
        assertEquals(null, attack(rule("r1", "a"), rule("r2", "b")))
    }

    @Test
    fun undermineOnADefeasiblePremise() {
        assertEquals("undermine", attack(rule("r1", "a"), premise("f1", "-a")))
        // an axiom is not undermined
        assertEquals(null, attack(rule("r1", "a"), "[[f1], none, [-a], [], [[], [], []]]"))
    }

    @Test
    fun contraryAttacksOnTheContraryPremisesOfATopRule() {
        assertEquals("contrary_rebut", attack(rule("r1", "a"), rule("r2", "b", "[~(a)]")))
        assertEquals("contrary_undermine", attack(premise("f1", "a"), rule("r2", "b", "[~(a)]")))
    }

    @Test
    fun undercutOnADefeasibleTopRule() {
        assertEquals("undercut", attack(rule("r1", "undercut(r2)"), rule("r2", "b")))
        // a statement conflicting with the identifier of the rule undercuts it as well
        assertEquals("undercut", attack(rule("r1", "-r2"), rule("r2", "b")))
        assertEquals(null, attack(rule("r1", "undercut(s2)"), strict("s2", "b")))
    }

    @Test
    fun aConflictCopyAttacksThroughItsConflictOnly() {
        val copy = "[[mc(1, conflict(a, b)), r1], r1, [a], [], [[r1], [r1], []]]"
        assertEquals("rebut", attack(copy, rule("r2", "b")))
        // not through the built-in conflict of a with -a
        assertEquals(null, attack(copy, rule("r2", "-a")))
        assertTrue(module.holds("attackRelation::conflictsWith($copy, [b])"))
        assertFalse(module.holds("attackRelation::conflictsWith($copy, [-a])"))
    }
}
