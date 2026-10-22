package it.unibo.tuprolog.argumentation.core.libs.language

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class ConflictsTest {
    @Test
    fun aLiteralConflictsWithItsNegation() {
        val module = ModuleTesting()
        assertTrue(module.holds("conflict::between([a], [-a])"))
        assertTrue(module.holds("conflict::between([-a], [a])"))
        assertFalse(module.holds("conflict::between([a], [b])"))
        // the guard of the built-in conflict keeps a double negation out
        assertFalse(module.holds("conflict::between([-a], [-(-(a))])"))
    }

    @Test
    fun obligationsAndPermissionsConflict() {
        val module = ModuleTesting()
        assertTrue(module.holds("conflict::between([o(a)], [o(-a)])"))
        assertTrue(module.holds("conflict::between([p(a)], [o(-a)])"))
        assertTrue(module.holds("conflict::between([o(a)], [p(-a)])"))
        assertTrue(module.holds("conflict::between([o(a)], [-p(a)])"))
        assertTrue(module.holds("conflict::between([sup(r1, r2)], [sup(r2, r1)])"))
    }

    @Test
    fun theTheoryDeclaresConflicts() {
        val module = ModuleTesting("conflict([a], [b]).\nconflict([c], [d], fail).")
        assertTrue(module.holds("conflict::between([a], [b])"))
        assertTrue(module.holds("conflict::unguarded([a], [b])"))
        // a conflict whose guard does not hold is no conflict, but it is a candidate one
        assertFalse(module.holds("conflict::between([c], [d])"))
        assertTrue(module.holds("conflict::guarded([c], [d], _)"))
    }

    @Test
    fun aConflictTermRelatesItsStatements() {
        val module = ModuleTesting()
        assertEquals(listOf("[b]"), module.values("conflict::stated(conflict(a, b), [a], X)", "X"))
        assertTrue(module.holds("conflict::stated(conflict(a, b, true), [a], [b])"))
        assertFalse(module.holds("conflict::stated(conflict(a, b, fail), [a], [b])"))
    }

    @Test
    fun withMetaConflictsOnlyTheConcludedConflictsHold() {
        val module = ModuleTesting("metaConflicts.\nr1 : [] => conflict(a, b).\nr2 : [] => conflict(c, d, true).")
        module.run("context_reset", "parser:::convertAllRules(_)", "standard_af:::buildArguments")
        assertTrue(module.holds("conflict::between([a], [b])"))
        assertTrue(module.holds("conflict::between([c], [d])"))
        // the built-in conflicts are off
        assertFalse(module.holds("conflict::between([a], [-a])"))
        assertEquals(2, module.solutions("conflict::statement([a], [b], _)").count { it.isYes })
    }
}
