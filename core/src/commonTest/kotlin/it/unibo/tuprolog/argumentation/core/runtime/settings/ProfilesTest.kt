package it.unibo.tuprolog.argumentation.core.runtime.settings

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.ModuleTesting
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class ProfilesTest {
    // a preference deciding a rebuttal: r1 is preferred to r2
    private val preferred = "r1 : [] => a.\nr2 : [] => -a.\nsup(r1, r2).\n"

    // The statements labelled in, out and und, by a solver with the default settings
    private fun labels(
        theory: String,
        settings: FlagsBuilder = FlagsBuilder(),
    ): String? {
        val solver = Arg2pSolverFactory.default(theory = theory, settings = settings.create())
        val solution = solver.solve(Struct.parse("buildLabelSets(I, O, U), L = [I, O, U]", solver.operators)).first()
        return solution
            .takeIf { it.isYes }
            ?.substitution
            ?.getByName("L")
            ?.toString()
    }

    @Test
    fun theProfileFreePresetsNothing() {
        // the solver of the tests weighs preferences (standardPref)
        assertEquals("[[[a]], [['-'(a)]], []]", labels(preferred))
        assertEquals(labels(preferred), labels(preferred + "graphBuildMode(free).\n"))
    }

    @Test
    fun aspicPlusWeighsPreferencesAndAspicDoesNot() {
        assertEquals("[[[a]], [['-'(a)]], []]", labels(preferred + "graphBuildMode(aspic_plus).\n"))
        assertEquals("[[], [], [['-'(a)], [a]]]", labels(preferred + "graphBuildMode(aspic).\n"))
    }

    @Test
    fun aFlagTheTheoryStatesOverridesItsProfile() {
        assertEquals("[[[a]], [['-'(a)]], []]", labels(preferred + "graphBuildMode(aspic).\ngraphExtension(standardPref).\n"))
    }

    @Test
    fun theSolverSettingsChooseAProfileAsWell() {
        assertEquals("[[], [], [['-'(a)], [a]]]", labels(preferred, FlagsBuilder().graphBuildMode("aspic")))
    }

    @Test
    fun theProfileMetaReasonsAboutRulesAndConflicts() {
        val module = ModuleTesting("graphBuildMode(meta).\n")
        assertTrue(module.holds("setting(metaRules)"))
        assertTrue(module.holds("setting(metaConflicts)"))
        assertEquals(listOf("defeasiblePref"), module.values("setting(graphExtension, X)", "X"))
        assertFalse(ModuleTesting("").holds("setting(metaRules)"))
    }

    @Test
    fun theDefeasiblePartOfAspicRefusesStrictRules() {
        assertEquals("[[[a]], [['-'(a)]], []]", labels(preferred + "graphBuildMode(aspic_defeasible).\n"))
        val theory = preferred + "s1 : [] -> b.\ngraphBuildMode(aspic_defeasible).\n"
        val solver = Arg2pSolverFactory.default(theory = theory, settings = FlagsBuilder().create())
        val solution = solver.solve(Struct.parse("buildLabelSets(_, _, _)", solver.operators)).first()
        assertTrue(solution.isHalt, solution.toString())
    }

    @Test
    fun anAbstractTheoryStatesItsArgumentsAndAttacks() {
        val theory = "graphBuildMode(abstract).\narg(d).\natt(a, b).\natt(b, c).\n"
        assertEquals("[[[d], [c], [a]], [[b]], []]", labels(theory))
    }

    @Test
    fun anAbstractTheoryDerivesItsAttacksWithProlog() {
        val theory = "graphBuildMode(abstract).\nbeats(x, y).\nbeats(y, z).\nbeats(w, w).\natt(X, Y) :- beats(X, Y).\n"
        assertEquals("[[[z], [x]], [[y]], [[w]]]", labels(theory))
    }
}
