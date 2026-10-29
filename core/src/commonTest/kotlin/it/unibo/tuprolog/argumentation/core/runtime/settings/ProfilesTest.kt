package it.unibo.tuprolog.argumentation.core.runtime.settings

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.ModuleTesting
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.Solution
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class ProfilesTest {
    // a preference deciding a rebuttal: r1 is preferred to r2
    private val preferred = "r1 : [] => a.\nr2 : [] => -a.\nsup(r1, r2).\n"

    // The first solution of buildLabelSets, by a solver with the default settings
    private fun labelling(
        theory: String,
        settings: FlagsBuilder = FlagsBuilder(),
    ): Solution {
        val solver = Arg2pSolverFactory.default(theory = theory, settings = settings.create())
        return solver.solve(Struct.parse("buildLabelSets(I, O, U), L = [I, O, U]", solver.operators)).first()
    }

    // The first solution of answerQuery for Goal, in query mode
    private fun answer(
        theory: String,
        goal: String,
    ): Solution {
        val solver = Arg2pSolverFactory.default(theory = theory, settings = FlagsBuilder().queryMode(true).create())
        return solver.solve(Struct.parse("answerQuery($goal, I, O, U), L = [I, O, U]", solver.operators)).first()
    }

    // The statements labelled in, out and und
    private fun labels(
        theory: String,
        settings: FlagsBuilder = FlagsBuilder(),
    ): String? =
        labelling(theory, settings)
            .takeIf { it.isYes }
            ?.substitution
            ?.getByName("L")
            ?.toString()

    @Test
    fun aspicPlusIsTheDefault() {
        assertEquals(Profiles.ASPIC_PLUS.name, FlagsBuilder().graphBuildMode)
        val module = ModuleTesting("")
        assertTrue(module.holds("setting(autoTransposition)"))
        assertEquals(listOf("standardPref", "rebutRestriction"), module.values("setting(graphExtension, X)", "X"))
        assertEquals("[[[a]], [['-'(a)]], []]", labels(preferred))
    }

    @Test
    fun theProfileFreePresetsNothing() {
        // the flags as the solver and the theory state them, their defaults otherwise
        val module = ModuleTesting("graphBuildMode(free).\n")
        assertFalse(module.holds("setting(autoTransposition)"))
        assertEquals(listOf("standardPref"), module.values("setting(graphExtension, X)", "X"))
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
    }

    @Test
    fun theDefeasiblePartOfAspicRefusesStrictRules() {
        assertEquals("[[[a]], [['-'(a)]], []]", labels(preferred + "graphBuildMode(aspic_defeasible).\n"))
        val refused = labelling(preferred + "s1 : [] -> b.\ngraphBuildMode(aspic_defeasible).\n")
        assertTrue(refused.isHalt, refused.toString())
        // nor premises: a fact is a rule with no premise
        val premise = labelling(preferred + "f1 :=> b.\ngraphBuildMode(aspic_defeasible).\n")
        assertTrue(premise.isHalt, premise.toString())
        val module = ModuleTesting("graphBuildMode(aspic_defeasible).\n")
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

    @Test
    fun theSettingsOfTheSolverOverrideTheProfile() {
        val solver = Arg2pSolverFactory.default(theory = "", settings = FlagsBuilder().metaRules(true).create())
        assertTrue(solver.solve(Struct.parse("setting(metaRules)", solver.operators)).first().isYes)
        // but a value the default of its flag states leaves the profile in charge
        val default = Arg2pSolverFactory.default(theory = "", settings = FlagsBuilder(autoTransposition = false).create())
        assertTrue(default.solve(Struct.parse("setting(autoTransposition)", default.operators)).first().isYes)
    }

    @Test
    fun aTheoryClearsWhatItsProfilePresets() {
        val module = ModuleTesting("graphExtension(none).\nautoTransposition(off).\n")
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
        val module = ModuleTesting(aba)
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
        val defeasible = labelling("$aba\nr3 : b => z.\n")
        assertTrue(defeasible.isHalt, defeasible.toString())
        val notFlat = labelling("$aba\nr3 : x -> a.\n")
        assertTrue(notFlat.isHalt, notFlat.toString())
    }

    @Test
    fun anAbaTheoryHasNoOrdinaryPremise() {
        // its premises are its assumptions: an ordinary premise is refused, an axiom (a strict fact) is not
        val premise = labelling("graphBuildMode(aba).\nf1 :=> d.\nf2 :=> -d.\n")
        assertTrue(premise.isHalt, premise.toString())
        assertTrue(premise.exception.toString().contains("ordinary premises"), premise.exception.toString())
        assertEquals("[[[d]], [], []]", labels("graphBuildMode(aba).\nf1 :-> d.\n"))
    }

    @Test
    fun anAbstractTheoryHasNoRule() {
        // rules, premises and axioms of the structured notations are refused, not dropped nor read as attacks
        for (theory in listOf("f1 :=> d.\n", "r1 : [] => d.\n", "r1 : [] -> d.\nr2 : [] -> -d.\n", "f1 :-> d.\n", "assumption(a).\n")) {
            val refused = labelling("graphBuildMode(abstract).\n$theory")
            assertTrue(refused.isHalt, "$theory: $refused")
        }
        // the notation of the profile, its Prolog clauses and its arguments of any shape are not
        assertEquals("[[[f(x)]], [[g(y)]], []]", labels("graphBuildMode(abstract).\nf(x) -> g(y).\n"))
        assertEquals("[[[b], [a]], [], []]", labels("graphBuildMode(abstract).\nthing(a).\nthing(b).\narg(X) :- thing(X).\n"))
    }

    @Test
    fun anAbstractTheoryIsCheckedAndLabelledInQueryMode() {
        // query mode builds the graph through no builder of the profile: its language checks the theory
        for (theory in listOf("queryMode.\nf1 :=> d.\n", "f1 :=> d.\n")) {
            val refused = answer("graphBuildMode(abstract).\n$theory", "d")
            assertTrue(refused.isHalt, "$theory: $refused")
            assertTrue(refused.exception.toString().contains("abstract"), refused.exception.toString())
        }
        // and labelled as in the whole graph
        val theory = "graphBuildMode(abstract).\nqueryMode.\na -> b.\nb -> c.\n"
        assertEquals("[[c], [], []]", answer(theory, "c").substitution.getByName("L").toString())
        assertEquals("[[], [b], []]", answer(theory, "b").substitution.getByName("L").toString())
    }

    @Test
    fun anAbstractTheoryHasNoPreferenceConflictNorBurden() {
        // arg2p reads them in every theory, and an abstract framework would drop them
        for (theory in listOf("sup(a, b).\n", "conflict([a], [b]).\n", "conflict([a], [b], true).\n", "bp(a).\n")) {
            val refused = labelling("graphBuildMode(abstract).\na.\nb.\n$theory")
            assertTrue(refused.isHalt, "$theory: $refused")
        }
        // other facts are data for the clauses: they state no argument (an atom or arg/1 does)
        assertEquals("[[[a]], [[b]], []]", labels("graphBuildMode(abstract).\no(a).\nwins(a, b).\nX -> Y :- wins(X, Y).\n"))
        // arg/1 and att/2 take arguments of any shape; in the notation, a list reads as the premises of a rule
        assertEquals("[[[[a]]], [[b]], []]", labels("graphBuildMode(abstract).\natt([a], b).\n"))
        assertTrue(labelling("graphBuildMode(abstract).\n[a] -> b.\n").isHalt)
        // an argument may have the name of a burden or of a preference
        assertEquals("[[[bp]], [[sup]], []]", labels("graphBuildMode(abstract).\nbp -> sup.\n"))
    }
}
