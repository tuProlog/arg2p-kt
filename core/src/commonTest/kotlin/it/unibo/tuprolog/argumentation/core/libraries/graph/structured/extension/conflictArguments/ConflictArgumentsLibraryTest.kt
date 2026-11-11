package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.conflictArguments

import it.unibo.tuprolog.argumentation.core.LibraryTesting
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class ConflictArgumentsLibraryTest {
    private val copy = "[[mc(1, conflict(a, b)), r1], r1, [a], [], [[r1], [r1], []]]"
    private val plain = "[[r1], r1, [a], [], [[r1], [r1], []]]"

    @Test
    fun withoutMetaConflictsEveryArgumentAttacks() {
        val module = LibraryTesting()
        assertTrue(module.holds("conflict::attacker($plain)"))
    }

    @Test
    fun withMetaConflictsOnlyTheCopiesAttack() {
        val module = LibraryTesting("metaConflicts.")
        assertTrue(module.holds("conflict::attacker($copy)"))
        assertFalse(module.holds("conflict::attacker($plain)"))
        assertEquals("conflict(a, b)", module.value("conflict::carried($copy, C)", "C"))
        assertFalse(module.holds("conflict::carried($plain, _)"))
    }

    @Test
    fun aCopyIsMadeForEachConflictArgument() {
        val module = LibraryTesting("metaConflicts.\nr1 : [] => a.\nr2 : [] => b.\nr3 : [] => conflict(a, b).\nr4 : [] => conflict(a, c).")
        module.run(
            "contextReset",
            "store::rulesRead",
            "ruleGraph:::buildArguments",
            "conflictArguments::buildCopies",
        )
        val conflicts = module.values("store::argumentFor([a], A), conflict::carried(A, C)", "C").toSet()
        assertEquals(setOf("conflict(a, b)", "conflict(a, c)"), conflicts)
        // each copy is supported by its conflict argument
        assertTrue(
            module.holds(
                "store::argumentFor([a], A), conflict::carried(A, conflict(a, b)), " +
                    "store::argumentFor([conflict(a, b)], C), contextCheck(support(C, A))",
            ),
        )
    }

    // The exploration of the base graph knows nothing of argued conflicts: this step declares what they relate
    private fun theStepExploresTheSidesOf(conflict: String) {
        val module = LibraryTesting.parsed("metaConflicts.\nqueryMode.\nr0 : [] => $conflict.\nf1 :=> a.\nf2 :=> b.")
        module.run("queryGraph::explore($conflict)")
        assertTrue(module.holds("store::argumentFor([$conflict], _)"))
        assertFalse(module.holds("store::argumentFor([a], _)"))
        assertFalse(module.holds("store::argumentFor([b], _)"))
        module.run("pipeline::steps(Steps), queryGraph::extend(Steps, directional)")
        assertTrue(module.holds("store::argumentFor([a], _)"))
        assertTrue(module.holds("store::argumentFor([b], _)"))
    }

    @Test
    fun theSidesOfAConflictAreExploredByTheStep() = theStepExploresTheSidesOf("conflict(a, b)")

    @Test
    fun theSidesOfAGuardedConflictAreExploredByTheStep() = theStepExploresTheSidesOf("conflict(a, b, 1 < 2)")
}
