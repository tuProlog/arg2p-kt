package it.unibo.tuprolog.argumentation.core.runtime.settings

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/**
 * The flags of a solver, checked through setting/1,2 (see Settings): the values of the builder, overridden flag by
 * flag by those the theory states, and errors for the flags no library declares.
 */
class SettingsTest {
    private fun solve(
        goal: String,
        theory: String = "",
        flags: FlagsBuilder = FlagsBuilder(),
    ): String {
        val solver = Arg2pSolverFactory.default(theory = theory, settings = flags.create())
        val solution = solver.solve(Struct.parse(goal, solver.operators)).first()
        return when {
            solution.isYes ->
                solution.substitution.entries
                    .filter { it.key.name == "L" }
                    .joinToString { it.value.toString() }
            solution.isHalt -> "error: " + solution.exception!!.message
            else -> "no"
        }
    }

    @Test
    fun valuesOfTheBuilder() {
        val grounded = FlagsBuilder().argumentLabellingMode("grounded")
        assertEquals("[grounded]", solve("findall(X, setting(argumentLabellingMode, X), L)", flags = grounded))
        assertEquals("yes", solve("setting(queryMode), L = yes", flags = FlagsBuilder().queryMode(true)))
        assertEquals("no", solve("setting(queryMode)", flags = FlagsBuilder().queryMode(false)))
    }

    @Test
    fun theTheoryOverridesTheBuilder() {
        val flags = FlagsBuilder().argumentLabellingMode("grounded").graphExtensions(listOf("standardPref"))
        assertEquals("[preferred]", solve("findall(X, setting(argumentLabellingMode, X), L)", "argumentLabellingMode(preferred).", flags))
        assertEquals("[bp]", solve("findall(X, setting(graphExtension, X), L)", "graphExtension(bp).", flags))
        // a value of the builder the default of its flag states leaves the profile (aspic_plus) in charge
        assertEquals(
            "[standardPref, rebutRestriction]",
            solve("findall(X, setting(graphExtension, X), L)", "argumentLabellingMode(preferred).", flags),
        )
        assertEquals("yes", solve("setting(metaConflicts), L = yes", "metaConflicts."))
    }

    @Test
    fun flagsWithManyValuesInOrder() {
        val flags = FlagsBuilder().empty(true)
        assertEquals("[a, b]", solve("findall(X, setting(graphExtension, X), L)", "graphExtension(a).\ngraphExtension(b).", flags))
        // none stated: those of the profile (aspic_plus)
        assertEquals("[standardPref, rebutRestriction]", solve("findall(X, setting(graphExtension, X), L)", "", flags))
        assertEquals("[]", solve("findall(X, setting(graphExtension, X), L)", "graphExtension(none).", flags))
    }

    @Test
    fun theConflictModelIsToldByItsParts() {
        val parts = "findall(X, setting(conflictModel, X), L)"
        assertEquals("[negation, deontic, preference, declared]", solve(parts))
        // builtin stands for all of them, each once, in the order stated
        assertEquals("[negation, deontic, preference, declared]", solve(parts, "conflictModel(builtin)."))
        assertEquals("[declared, negation, deontic, preference]", solve(parts, "conflictModel(declared).\nconflictModel(builtin)."))
        assertEquals("[declared]", solve(parts, "conflictModel(declared)."))
        assertEquals("[preference, negation]", solve(parts, "conflictModel(preference).\nconflictModel(negation)."))
        assertEquals("[]", solve(parts, "conflictModel(none)."))
        assertEquals("yes", solve("setting(conflictModel, deontic), L = yes", "conflictModel(builtin)."))
        assertEquals("no", solve("setting(conflictModel, builtin)", "conflictModel(builtin)."))
    }

    @Test
    fun theConflictModelOfAProfileStandsUnlessAnotherIsStated() {
        val parts = "findall(X, setting(conflictModel, X), L)"
        // ABA: the declared conflicts alone
        assertEquals("[declared]", solve(parts, "graphBuildMode(aba)."))
        assertEquals("[declared]", solve(parts, "graphBuildMode(aba).", FlagsBuilder().empty(true)))
        // the theory states others
        assertEquals("[negation, declared]", solve(parts, "graphBuildMode(aba).\nconflictModel(negation).\nconflictModel(declared)."))
    }

    @Test
    fun switchesOfTheBuilder() {
        val meta = FlagsBuilder().metaConflicts(true).metaRules(true)
        assertEquals("yes", solve("setting(metaConflicts), setting(metaRules), L = yes", flags = meta))
        assertEquals("no", solve("setting(metaConflicts)"))
    }

    @Test
    fun undeclaredFlagsAreErrors() {
        assertTrue(solve("setting(noSuchFlag)").startsWith("error"))
        assertTrue(solve("setting(noSuchFlag, _)").startsWith("error"))
        assertTrue(solve("setting(queryMode, _)").startsWith("error"))
        assertTrue(solve("setting(graphBuildMode)").startsWith("error"))
        assertTrue(solve("setting(_)").startsWith("error"))
    }
}
