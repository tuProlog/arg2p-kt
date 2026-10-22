package it.unibo.tuprolog.argumentation.core.libs.graph.attack

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class AttackCandidatesTest {
    private val b = "[[r2], r2, [b], [~(c)], [[r2], [r2], []]]"

    private fun module() =
        ModuleTesting("r1 : [] => -b.\nr2 : ~(c) => b.\nr3 : ~(b) => d.").also {
            it.run("context_reset", "parser:::convertAllRules(_)")
        }

    @Test
    fun theAttackersOfAnArgumentConcludeConflictsOrUndercuts() {
        val candidates = module().values("attackCandidates::findPossibleAttackers($b, S)", "S")
        // the conflicting conclusion, the contrary premise, the undercut of the top rule
        assertTrue("['-'(b)]" in candidates, "$candidates")
        assertTrue("[c]" in candidates, "$candidates")
        assertTrue("[undercut(r2)]" in candidates, "$candidates")
    }

    @Test
    fun theAttackedOfAnArgumentAreTheConverse() {
        val attacked = module().values("attackCandidates::possiblyAttacked($b, S)", "S").toSet()
        // the conflicting statement, and the conclusion of the rule with b as a contrary premise
        assertTrue("'-'(b)" in attacked, "$attacked")
        assertTrue("d" in attacked, "$attacked")
    }

    @Test
    fun theAttackersAreLookedForOnce() {
        val module = module()
        module.run("context_id(argument($b), Id), findall(S, attackCandidates::possibleAttackers(Id, $b, S), _)")
        assertTrue(module.holds("context_id(argument($b), Id), context_check(possibleAttackersOf(Id, _))"))
        assertEquals(
            module.values("attackCandidates::findPossibleAttackers($b, S)", "S"),
            module.values("context_id(argument($b), Id), attackCandidates::possibleAttackers(Id, $b, S)", "S"),
        )
    }
}
