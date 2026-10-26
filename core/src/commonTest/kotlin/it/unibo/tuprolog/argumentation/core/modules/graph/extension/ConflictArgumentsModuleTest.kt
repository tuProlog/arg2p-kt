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
        assertTrue(module.holds("conflictArguments::canAttack($plain)"))
    }

    @Test
    fun withMetaConflictsOnlyTheCopiesAttack() {
        val module = ModuleTesting("metaConflicts.")
        assertTrue(module.holds("conflictArguments::canAttack($copy)"))
        assertFalse(module.holds("conflictArguments::canAttack($plain)"))
        assertEquals("conflict(a, b)", module.value("conflictArguments::conflictOf($copy, C)", "C"))
        assertFalse(module.holds("conflictArguments::conflictOf($plain, _)"))
    }

    @Test
    fun aCopyIsMadeForEachConflictArgument() {
        val module = ModuleTesting("metaConflicts.\nr1 : [] => a.\nr2 : [] => b.\nr3 : [] => conflict(a, b).\nr4 : [] => conflict(a, c).")
        module.run("context_reset", "parser:::convertAllRules(_)", "ruleGraph:::buildArguments", "ruleGraph::buildAdditionalArguments")
        val conflicts = module.values("graphStore::argumentFor([a], A), conflictArguments::conflictOf(A, C)", "C").toSet()
        assertEquals(setOf("conflict(a, b)", "conflict(a, c)"), conflicts)
        // each copy is supported by its conflict argument
        assertTrue(
            module.holds(
                "graphStore::argumentFor([a], A), conflictArguments::conflictOf(A, conflict(a, b)), " +
                    "graphStore::argumentFor([conflict(a, b)], C), context_check(support(C, A))",
            ),
        )
    }
}
