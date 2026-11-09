package it.unibo.tuprolog.argumentation.core.modules.graph.extension

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class ConflictArgumentsModuleTest {
    private val copy = "[[mc(1, conflict(a, b)), r1], r1, [a], [], [[r1], [r1], []]]"
    private val plain = "[[r1], r1, [a], [], [[r1], [r1], []]]"

    @Test
    fun withoutMetaConflictsEveryArgumentAttacks() {
        val module = ModuleTesting()
        assertTrue(module.holds("conflict::attacker($plain)"))
    }

    @Test
    fun withMetaConflictsOnlyTheCopiesAttack() {
        val module = ModuleTesting("metaConflicts.")
        assertTrue(module.holds("conflict::attacker($copy)"))
        assertFalse(module.holds("conflict::attacker($plain)"))
        assertEquals("conflict(a, b)", module.value("conflict::carried($copy, C)", "C"))
        assertFalse(module.holds("conflict::carried($plain, _)"))
    }

    @Test
    fun aCopyIsMadeForEachConflictArgument() {
        val module = ModuleTesting("metaConflicts.\nr1 : [] => a.\nr2 : [] => b.\nr3 : [] => conflict(a, b).\nr4 : [] => conflict(a, c).")
        module.run(
            "context_reset",
            "parser:::convertAllRules(_)",
            "ruleGraph:::buildArguments",
            "conflictArguments::buildAdditionalArguments",
        )
        val conflicts = module.values("graphStore::argumentFor([a], A), conflict::carried(A, C)", "C").toSet()
        assertEquals(setOf("conflict(a, b)", "conflict(a, c)"), conflicts)
        // each copy is supported by its conflict argument
        assertTrue(
            module.holds(
                "graphStore::argumentFor([a], A), conflict::carried(A, conflict(a, b)), " +
                    "graphStore::argumentFor([conflict(a, b)], C), context_check(support(C, A))",
            ),
        )
    }

    // The exploration of the base graph knows nothing of argued conflicts: this step declares what they relate
    private fun theStepExploresTheSidesOf(conflict: String) {
        val module = ModuleTesting.parsed("metaConflicts.\nqueryMode.\nr0 : [] => $conflict.\nf1 :=> a.\nf2 :=> b.")
        module.run("queryExploration::explore($conflict)")
        assertTrue(module.holds("graphStore::argumentFor([$conflict], _)"))
        assertFalse(module.holds("graphStore::argumentFor([a], _)"))
        assertFalse(module.holds("graphStore::argumentFor([b], _)"))
        module.run("queryExploration::extend(directional)")
        assertTrue(module.holds("graphStore::argumentFor([a], _)"))
        assertTrue(module.holds("graphStore::argumentFor([b], _)"))
    }

    @Test
    fun theSidesOfAConflictAreExploredByTheStep() = theStepExploresTheSidesOf("conflict(a, b)")

    @Test
    fun theSidesOfAGuardedConflictAreExploredByTheStep() = theStepExploresTheSidesOf("conflict(a, b, 1 < 2)")
}
