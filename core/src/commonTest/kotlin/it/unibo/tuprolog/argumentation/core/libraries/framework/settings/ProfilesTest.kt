package it.unibo.tuprolog.argumentation.core.libraries.framework.settings

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.Profile
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.LibraryTesting
import it.unibo.tuprolog.argumentation.core.libraries.framework.settings.flags.GraphBuildMode
import it.unibo.tuprolog.argumentation.core.libraries.framework.settings.helpers.Profiles
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.ParseException
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.Solution
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class ProfilesTest {
    // a preference deciding a rebuttal: r1 is preferred to r2
    private val preferred = "r1 : [] => a.\nr2 : [] => -a.\nsup(r1, r2).\n"

    // The first solution of solve, by a solver with the default settings
    private fun labelling(
        theory: String,
        settings: Flags = Flags.defaults(),
    ): Solution {
        val solver = Arg2pSolverFactory.default(theory = theory, settings = settings.library())
        return solver.solve(Struct.parse("solve(I, O, U), L = [I, O, U]", solver.operators)).first()
    }

    // The first solution of solve for Goal, in query mode
    // [theory] does not read: written with a notation its languages do not have (see TheoryNotation)
    private fun assertDoesNotRead(theory: String) {
        val failure = runCatching { Arg2pSolverFactory.default(theory = theory, settings = Flags.defaults().library()) }.exceptionOrNull()
        assertTrue(failure is ParseException, "$theory: $failure")
    }

    private fun answer(
        theory: String,
        goal: String,
    ): Solution {
        val solver = Arg2pSolverFactory.default(theory = theory, settings = Flags.defaults().queryMode(true).library())
        return solver.solve(Struct.parse("solve($goal, I, O, U), L = [I, O, U]", solver.operators)).first()
    }

    // The statements labelled in, out and und
    private fun labels(
        theory: String,
        settings: Flags = Flags.defaults(),
    ): String? =
        labelling(theory, settings)
            .takeIf { it.isYes }
            ?.substitution
            ?.getByName("L")
            ?.toString()

    @Test
    fun aspicPlusIsTheDefault() {
        assertEquals(Profile.ASPIC_PLUS.value, GraphBuildMode.default())
        assertEquals(Profiles.all.map { it.name }, GraphBuildMode.values())
        val module = LibraryTesting("")
        assertTrue(module.holds("setting(autoTransposition)"))
        assertEquals(listOf("standardPref", "rebutRestriction"), module.values("setting(graphExtension, X)", "X"))
        assertEquals("[[[a]], [['-'(a)]], []]", labels(preferred))
    }

    @Test
    fun theProfileFreePresetsNothing() {
        // the flags as the solver and the theory state them, their defaults otherwise
        val module = LibraryTesting("graphBuildMode(free).\n")
        assertFalse(module.holds("setting(autoTransposition)"))
        assertEquals(listOf("standardPref"), module.values("setting(graphExtension, X)", "X"))
    }

    @Test
    fun aspicPlusWeighsPreferencesAndAspicDoesNot() {
        assertEquals("[[], [], [['-'(a)], [a]]]", labels(preferred + "graphBuildMode(aspic).\n"))
    }

    @Test
    fun aFlagTheTheoryStatesOverridesItsProfile() {
        assertEquals("[[[a]], [['-'(a)]], []]", labels(preferred + "graphBuildMode(aspic).\ngraphExtension(standardPref).\n"))
    }

    @Test
    fun theSolverSettingsChooseAProfileAsWell() {
        assertEquals("[[], [], [['-'(a)], [a]]]", labels(preferred, Flags.parse("graphBuildMode(aspic).")))
    }

    @Test
    fun theProfileMetaReasonsAboutRulesAndConflicts() {
        val module = LibraryTesting("graphBuildMode(meta).\n")
        assertTrue(module.holds("setting(metaRules)"))
        assertTrue(module.holds("setting(metaConflicts)"))
        assertEquals(listOf("defeasiblePref"), module.values("setting(graphExtension, X)", "X"))
    }

    @Test
    fun aTheoryHasOnePreferenceModeAtMost() {
        val both = preferred + "graphExtension(standardPref).\ngraphExtension(defeasiblePref).\n"
        val refused = labelling(both)
        assertTrue(refused.isHalt, refused.toString())
        assertTrue(refused.exception.toString().contains("one_preference_mode"), refused.exception.toString())
        assertTrue(answer(both, "a").isHalt)
    }

    @Test
    fun abaPrefIsForAbaTheoriesAlone() {
        val refused = labelling(preferred + "graphExtension(abaPref).\n")
        assertTrue(refused.isHalt, refused.toString())
        assertTrue(refused.exception.toString().contains("aba_theory"), refused.exception.toString())
    }

    @Test
    fun theStepsChangingTheGraphApplyInTheirOrder() {
        // whatever the order the flags state them in: the arguments added first, then the attacks changed
        val module =
            LibraryTesting(
                "metaConflicts.\ngraphExtension(bp).\ngraphExtension(standardPref).\ngraphExtension(rebutRestriction).\ntemporal.\n",
            )
        assertEquals(
            listOf("constraints", "conflictArguments", "rebutRestriction", "standardPref", "bp"),
            module.values("pipeline::steps(Steps), member(S, Steps)", "S"),
        )
    }

    @Test
    fun theDefeasiblePartOfAspicReadsDefeasibleRulesAlone() {
        assertEquals("[[[a]], [['-'(a)]], []]", labels(preferred + "graphBuildMode(aspic_defeasible).\n"))
        // a strict rule is not read (written with the operators of Prolog, it is data for the clauses)
        assertEquals("[[[a]], [['-'(a)]], []]", labels(preferred + "s1 : [] -> b.\ngraphBuildMode(aspic_defeasible).\n"))
        // premises and axioms are no part of its notation: they do not read (a fact is a rule with no premise)
        assertDoesNotRead(preferred + "f1 :=> b.\ngraphBuildMode(aspic_defeasible).\n")
        assertDoesNotRead(preferred + "f1 :-> b.\ngraphBuildMode(aspic_defeasible).\n")
        val module = LibraryTesting("graphBuildMode(aspic_defeasible).\n")
        assertEquals(listOf("last"), module.values("setting(orderingPrinciple, X)", "X"))
        assertEquals(listOf("normal"), module.values("setting(orderingComparator, X)", "X"))
    }

    // ASPIC+ is well formed: a contrary (a conflict declared one way) targets no axiom nor conclusion of a strict rule
    @Test
    fun aspicRefusesContrariesOfStrictConclusions() {
        val strict = "f1 :-> b.\ns1 : b -> c.\nr1 : [] => d.\n"
        assertTrue(labelling(strict + "conflict([d], [c]).\n").isHalt)
        assertTrue(labelling(strict + "conflict([d], [b]).\n").isHalt)
        // on a defeasible conclusion, or both ways (a contradictory), it stands
        assertTrue(labelling(strict + "conflict([c], [d]).\n").isYes)
        assertTrue(labelling(strict + "conflict([d], [c]).\nconflict([c], [d]).\n").isYes)
        // and the profile free does not check it
        assertTrue(labelling(strict + "conflict([d], [c]).\ngraphBuildMode(free).\n").isYes)
    }

    // Predicates of the theory named as the helpers of the check, or of the guards of the conflicts, change neither
    @Test
    fun thePredicatesOfTheTheoryLeaveTheChecksAsTheyAre() {
        val strict = "f1 :-> b.\ns1 : b -> c.\nr1 : [] => d.\n"
        assertTrue(labelling(strict + "contrary(d, c).\ndeclared(d, c).\nstrictly(c).\n").isYes)
        assertTrue(labelling(strict + "holds(x).\nconflict([d], [e], fail).\nr2 : [] => e.\n").isYes)
    }

    @Test
    fun theSettingsOfTheSolverOverrideTheProfile() {
        val solver = Arg2pSolverFactory.default(theory = "", settings = Flags.defaults().metaRules(true).library())
        assertTrue(solver.solve(Struct.parse("setting(metaRules)", solver.operators)).first().isYes)
        // a switch stated off as well, over the profile presetting it on
        val off = Arg2pSolverFactory.default(theory = "", settings = Flags.defaults().autoTransposition(false).library())
        assertFalse(off.solve(Struct.parse("setting(autoTransposition)", off.operators)).first().isYes)
    }

    @Test
    fun aTheoryClearsWhatItsProfilePresets() {
        val module = LibraryTesting("graphExtension(none).\nautoTransposition(off).\n")
        assertTrue(module.values("setting(graphExtension, X)", "X").isEmpty())
        assertFalse(module.holds("setting(autoTransposition)"))
    }

    @Test
    fun anAbstractTheoryStatesItsArgumentsAndAttacks() {
        assertEquals("[[[d], [c], [a]], [[b]], []]", labels("graphBuildMode(abstract).\narg(d).\natt(a, b).\natt(b, c).\n"))
        // the notation of the profile: an argument, an attack, a mutual attack
        assertEquals("[[[d], [c], [a]], [[b]], []]", labels("graphBuildMode(abstract).\nd.\na -> b.\nb -> c.\n"))
        assertEquals("[[[c]], [[a]], [[d], [b]]]", labels("graphBuildMode(abstract).\nb <-> d.\nc -> a.\n"))
    }

    @Test
    fun anAbstractTheoryDerivesItsAttacksWithProlog() {
        val facts = "beats(x, y).\nbeats(y, z).\nbeats(w, w).\n"
        assertEquals("[[[z], [x]], [[y]], [[w]]]", labels("graphBuildMode(abstract).\n${facts}att(X, Y) :- beats(X, Y).\n"))
        assertEquals("[[[z], [x]], [[y]], [[w]]]", labels("graphBuildMode(abstract).\n${facts}X -> Y :- beats(X, Y).\n"))
    }

    // ABA: assumptions a and b; b is the contrary of a (one way: a does not attack b)
    private val aba = "graphBuildMode(aba).\nassumption(a).\nassumption(b).\ncontrary(a, x).\nr1 : b -> x.\nr2 : a -> y.\n"

    @Test
    fun anAbaTheoryAttacksAnAssumptionThroughItsContraryOnly() {
        assertEquals("[[[x], [b]], [[y], [a]], []]", labels(aba))
        // mutual contraries
        assertEquals("[[], [], [[y], [x], [b], [a]]]", labels(aba + "contrary(b, y).\n"))
        // the negation of an assumption is no contrary of it
        assertEquals("[[['-'(b)], [b]], [], []]", labels("graphBuildMode(aba).\nassumption(b).\nr1 : [] -> -b.\n"))
    }

    @Test
    fun theContrariesOfAbaHoldWhateverTheConflictModel() {
        // the profile presets the declared conflicts alone; the contraries hold with no part, or all of them
        val module = LibraryTesting(aba)
        assertEquals(listOf("declared"), module.values("setting(conflictModel, X)", "X"))
        assertEquals("[[[x], [b]], [[y], [a]], []]", labels(aba + "conflictModel(none).\n"))
        assertEquals("[[[x], [b]], [[y], [a]], []]", labels(aba + "conflictModel(builtin).\n"))
        // a negation conflicts when the theory states the part
        val negated = "graphBuildMode(aba).\nassumption(b).\nr1 : [] -> -b.\n"
        assertEquals("[[['-'(b)]], [[b]], []]", labels(negated + "conflictModel(negation).\n"))
    }

    @Test
    fun theConflictModelDecidesWhetherANegationConflicts() {
        assertEquals("[[[a]], [['-'(a)]], []]", labels(preferred + "conflictModel(negation).\n"))
        assertEquals("[[['-'(a)], [a]], [], []]", labels(preferred + "conflictModel(declared).\n"))
        assertEquals("[[['-'(a)], [a]], [], []]", labels(preferred + "conflictModel(none).\n"))
        assertEquals("[[[a]], [['-'(a)]], []]", labels(preferred + "conflictModel(builtin).\n"))
    }

    @Test
    fun anAbaTheoryIsFlatWithStrictRulesOnly() {
        // a defeasible rule is no part of its notation: it does not read
        assertDoesNotRead("$aba\nr3 : b => z.\n")
        val notFlat = labelling("$aba\nr3 : x -> a.\n")
        assertTrue(notFlat.isHalt, notFlat.toString())
    }

    @Test
    fun anAbaTheoryHasNoOrdinaryPremise() {
        // its premises are its assumptions: an ordinary premise does not read, an axiom (a strict fact) does
        assertDoesNotRead("graphBuildMode(aba).\nf1 :=> d.\nf2 :=> -d.\n")
        assertEquals("[[[d]], [], []]", labels("graphBuildMode(aba).\nf1 :-> d.\n"))
    }

    @Test
    fun anAbstractTheoryHasArgumentsOfAnyShape() {
        // the notation of the profile, its Prolog clauses and its arguments of any shape
        assertEquals("[[[f(x)]], [[g(y)]], []]", labels("graphBuildMode(abstract).\nf(x) -> g(y).\n"))
        assertEquals("[[[b], [a]], [], []]", labels("graphBuildMode(abstract).\nthing(a).\nthing(b).\narg(X) :- thing(X).\n"))
    }

    @Test
    fun anAbstractTheoryIsLabelledInQueryMode() {
        // as in the whole graph
        val theory = "graphBuildMode(abstract).\nqueryMode.\na -> b.\nb -> c.\n"
        assertEquals("[[c], [], []]", answer(theory, "c").substitution.getByName("L").toString())
        assertEquals("[[], [b], []]", answer(theory, "b").substitution.getByName("L").toString())
    }

    @Test
    fun anAbstractTheoryHasDataBesideItsFramework() {
        // facts other than arguments are data for the clauses: they state no argument (an atom or arg/1 does)
        assertEquals("[[[a]], [[b]], []]", labels("graphBuildMode(abstract).\no(a).\nwins(a, b).\nX -> Y :- wins(X, Y).\n"))
        // arg/1, att/2 and the notation take arguments of any shape
        assertEquals("[[[[a]]], [[b]], []]", labels("graphBuildMode(abstract).\natt([a], b).\n"))
        assertEquals("[[[[a]]], [[b]], []]", labels("graphBuildMode(abstract).\n[a] -> b.\n"))
        // an argument may have the name of a burden or of a preference
        assertEquals("[[[bp]], [[sup]], []]", labels("graphBuildMode(abstract).\nbp -> sup.\n"))
    }

    // The flags of ASPIC+ and those of ABA do not mix: a preference mode between rules, or the meta-argumentation,
    // on an ABA theory is refused
    @Test
    fun theFlagsOfAspicAndOfAbaAreExclusive() {
        val aba = "graphBuildMode(aba).\nassumption(a).\ncontrary(a, na).\nr1 : [] -> na.\n"
        assertEquals("[[[na]], [[a]], []]", labels(aba))
        listOf("graphExtension(standardPref).", "graphExtension(defeasiblePref).", "metaConflicts.", "metaRules.").forEach {
            val refused = labelling("$aba$it\n")
            assertTrue(refused.isHalt, "$it: $refused")
            assertTrue(refused.exception.toString().contains("aba"), "$it: ${refused.exception}")
        }
    }
}
