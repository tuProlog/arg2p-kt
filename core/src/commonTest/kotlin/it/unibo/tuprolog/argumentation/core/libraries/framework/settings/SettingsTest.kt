package it.unibo.tuprolog.argumentation.core.libraries.framework.settings

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.GraphExtension
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
        flags: Flags = Flags.defaults(),
    ): String {
        val solver = Arg2pSolverFactory.default(theory = theory, settings = flags.library())
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
    fun valuesOfTheFlags() {
        val grounded = Flags.parse("argumentLabellingMode(grounded).")
        assertEquals("[grounded]", solve("findall(X, setting(argumentLabellingMode, X), L)", flags = grounded))
        assertEquals("yes", solve("setting(queryMode), L = yes", flags = Flags.defaults().queryMode(true)))
        assertEquals("no", solve("setting(queryMode)", flags = Flags.defaults().queryMode(false)))
    }

    @Test
    fun theTheoryOverridesTheFlags() {
        val flags = Flags.parse("argumentLabellingMode(grounded).\ngraphExtension(standardPref).")
        assertEquals("[preferred]", solve("findall(X, setting(argumentLabellingMode, X), L)", "argumentLabellingMode(preferred).", flags))
        assertEquals("[bp]", solve("findall(X, setting(graphExtension, X), L)", "graphExtension(bp).", flags))
        // the flags stated win over the profile (aspicPlus)
        assertEquals("[standardPref]", solve("findall(X, setting(graphExtension, X), L)", "argumentLabellingMode(preferred).", flags))
        assertEquals("yes", solve("setting(metaConflicts), L = yes", "metaConflicts."))
    }

    @Test
    fun flagsWithManyValuesInOrder() {
        val flags = Flags.none()
        assertEquals("[a, b]", solve("findall(X, setting(graphExtension, X), L)", "graphExtension(a).\ngraphExtension(b).", flags))
        // none stated: those of the profile (aspicPlus)
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
        assertEquals("[declared]", solve(parts, "graphBuildMode(aba).", Flags.none()))
        // the theory states others
        assertEquals("[negation, declared]", solve(parts, "graphBuildMode(aba).\nconflictModel(negation).\nconflictModel(declared)."))
    }

    @Test
    fun switchesOfTheFlags() {
        val meta = Flags.defaults().metaConflicts(true).metaRules(true)
        assertEquals("yes", solve("setting(metaConflicts), setting(metaRules), L = yes", flags = meta))
        assertEquals("no", solve("setting(metaConflicts)"))
    }

    @Test
    fun aFlagNobodyDeclaresIsAnError() {
        assertTrue(solve("setting(queryMode)", flags = Flags.parse("queryMod.")).startsWith("error"))
    }

    @Test
    fun undeclaredFlagsAreErrors() {
        assertTrue(solve("setting(noSuchFlag)").startsWith("error"))
        assertTrue(solve("setting(noSuchFlag, _)").startsWith("error"))
        assertTrue(solve("setting(queryMode, _)").startsWith("error"))
        assertTrue(solve("setting(graphBuildMode)").startsWith("error"))
        assertTrue(solve("setting(_)").startsWith("error"))
    }

    // A value a closed flag does not declare, or a profile with no such name, is an error, not silently nothing
    @Test
    fun aValueAFlagDoesNotDeclareIsAnError() {
        val flags = Flags.none()
        assertTrue(solve("setting(orderingComparator, X)", "orderingComparator(elitst).", flags).startsWith("error"))
        assertTrue(solve("setting(graphExtension, X)", "graphBuildMode(aspic_plsu).", flags).startsWith("error"))
        // the modules of a user are values of the open flags
        assertEquals("[mine]", solve("findall(X, setting(graphExtension, X), L)", "graphExtension(mine).", flags))
    }

    // The flags of the solver win over the profile when stated, the theory over them, the default over nothing
    @Test
    fun theFlagsStatedWinOverTheProfileTheTheoryOverThem() {
        val stated = { theory: String, flags: Flags, goal: String ->
            val solver = Arg2pSolverFactory.default(theory = theory, settings = flags.library())
            val solution = solver.solve(Struct.parse(goal, solver.operators)).first()
            if (solution.isYes) solution.substitution.getByName("L")?.toString() ?: "yes" else "no"
        }
        // aspicPlus presets transposition on: the flag given turns it off, the theory turns it on again
        assertEquals("no", stated("", Flags.defaults().autoTransposition(false), "setting(autoTransposition)"))
        assertEquals("yes", stated("autoTransposition.", Flags.defaults().autoTransposition(false), "setting(autoTransposition)"))
        // the extensions given are those in effect, not the profile's
        assertEquals(
            "[standardPref]",
            stated("", Flags.defaults().graphExtensions(GraphExtension.STANDARD_PREF), "findall(X, setting(graphExtension, X), L)"),
        )
        // nothing stated: query mode is on by default, with the flags parsed from text as well
        assertEquals("yes", stated("", Flags.defaults(), "setting(queryMode)"))
        assertEquals("yes", stated("", Flags.parse("argumentLabellingMode(preferred).\n"), "setting(queryMode)"))
        assertEquals("no", stated("", Flags.parse("queryMode(off).\n"), "setting(queryMode)"))
    }
}
