package it.unibo.tuprolog.argumentation.core.modules.graph.language

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class ConflictModuleTest {
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

    // A conflict of each part of the conflict model: negation, deontic, preference and declared
    private val conflicts =
        mapOf(
            "negation" to "([a], [-a])",
            "deontic" to "([o(a)], [p(-a)])",
            "preference" to "([sup(r1, r2)], [sup(r2, r1)])",
            "declared" to "([c], [d])",
        )

    // The parts of the conflict model whose conflict holds, for a theory stating [model] (and declaring conflict([c], [d]))
    private fun partsHolding(model: String): List<String> {
        val module = ModuleTesting("$model\nconflict([c], [d]).")
        return conflicts.filter { (_, conflict) -> module.holds("conflict::between$conflict") }.keys.toList()
    }

    @Test
    fun eachPartOfTheConflictModelHoldsAlone() {
        for (part in conflicts.keys) assertEquals(listOf(part), partsHolding("conflictModel($part)."), part)
        assertEquals(listOf("negation", "declared"), partsHolding("conflictModel(negation).\nconflictModel(declared)."))
        // the deontic part has every conflict between obligations and permissions, not those of their negation
        val deontic = ModuleTesting("conflictModel(deontic).")
        val obligations = listOf("([o(a)], [o(-a)])", "([p(a)], [o(-a)])", "([o(a)], [p(-a)])", "([o(a)], [-p(a)])", "([-p(a)], [o(a)])")
        for (conflict in obligations) assertTrue(deontic.holds("conflict::between$conflict"), conflict)
        assertFalse(deontic.holds("conflict::between([p(a)], [-p(a)])"))
    }

    @Test
    fun theConflictModelHasEveryPartByDefaultAndBuiltinStandsForThem() {
        val all = conflicts.keys.toList()
        assertEquals(all, partsHolding(""))
        assertEquals(all, partsHolding("conflictModel(builtin)."))
        assertEquals(
            all,
            partsHolding("conflictModel(negation).\nconflictModel(deontic).\nconflictModel(preference).\nconflictModel(declared)."),
        )
    }

    @Test
    fun theDeclaredConflictModelKeepsTheDeclaredConflictsAlone() {
        assertEquals(listOf("declared"), partsHolding("conflictModel(declared)."))
        val module = ModuleTesting("conflictModel(declared).\nconflict([a], [b]).\nconflict([c], [d], fail).")
        assertTrue(module.holds("conflict::unguarded([a], [b])"))
        assertTrue(module.holds("conflict::guarded([c], [d], _)"))
        assertFalse(module.holds("conflict::guarded([a], [-a], _)"))
    }

    @Test
    fun aConflictModelWithNoPartHasNoConflict() {
        assertEquals(emptyList(), partsHolding("conflictModel(none)."))
        val module = ModuleTesting("conflictModel(none).\nconflict([a], [b]).\nconflict([c], [d], true).")
        assertFalse(module.holds("conflict::unguarded([a], [b])"))
        assertFalse(module.holds("conflict::guarded(_, _, _)"))
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
        module.run("context_reset", "parser:::convertAllRules(_)", "ruleGraph:::buildArguments")
        assertTrue(module.holds("conflict::between([a], [b])"))
        assertTrue(module.holds("conflict::between([c], [d])"))
        // the built-in conflicts are off
        assertFalse(module.holds("conflict::between([a], [-a])"))
        assertEquals(2, module.solutions("conflict::statement([a], [b], _)").count { it.isYes })
    }

    @Test
    fun withMetaConflictsTheConflictModelDoesNotCount() {
        for (model in listOf("conflictModel(negation).", "conflictModel(declared).", "conflictModel(none).")) {
            val module = ModuleTesting("metaConflicts.\n$model\nr1 : [] => conflict(a, b).\nconflict([c], [d]).")
            module.run("context_reset", "parser:::convertAllRules(_)", "ruleGraph:::buildArguments")
            assertTrue(module.holds("conflict::between([a], [b])"), model)
            assertFalse(module.holds("conflict::between([a], [-a])"), model)
            assertFalse(module.holds("conflict::between([c], [d])"), model)
        }
    }
}
