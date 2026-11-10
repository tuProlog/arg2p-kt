package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.attack.attackSearch

import it.unibo.tuprolog.argumentation.core.LibraryTesting
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class AttackSearchLibraryTest {
    private val b = "[[r2], r2, [b], [~(c)], [[r2], [r2], []]]"

    private fun module() = LibraryTesting.parsed("r1 : [] => -b.\nr2 : ~(c) => b.\nr3 : ~(b) => d.")

    @Test
    fun theAttackersOfAnArgumentConcludeConflictsOrUndercuts() {
        val candidates = module().values("attackSearch::findPossibleAttackers($b, S)", "S")
        // the conflicting conclusion, the contrary premise, the undercut of the top rule
        assertTrue("['-'(b)]" in candidates, "$candidates")
        assertTrue("[c]" in candidates, "$candidates")
        assertTrue("[undercut(r2)]" in candidates, "$candidates")
    }

    @Test
    fun theAttackedOfAnArgumentAreTheConverse() {
        val attacked = module().values("attackSearch::possiblyAttacked($b, S)", "S").toSet()
        // the conflicting statement, and the conclusion of the rule with b as a contrary premise
        assertTrue("'-'(b)" in attacked, "$attacked")
        assertTrue("d" in attacked, "$attacked")
    }

    @Test
    fun theAttackersAreLookedForOnce() {
        val module = module()
        module.run("context_id(argument($b), Id), findall(S, attackSearch::possibleAttackers(Id, $b, S), _)")
        assertTrue(module.holds("context_id(argument($b), Id), context_check(possibleAttackersOf(Id, _))"))
        assertEquals(
            module.values("attackSearch::findPossibleAttackers($b, S)", "S"),
            module.values("context_id(argument($b), Id), attackSearch::possibleAttackers(Id, $b, S)", "S"),
        )
    }
}
