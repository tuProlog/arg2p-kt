package it.unibo.tuprolog.argumentation.core.modules.graph.language

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.ModuleTesting
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class TranspositionModuleTest {
    private val rules = "[[s1, (a, b), c]]"

    @Test
    fun eachPremiseIsConcludedNegatedFromTheNegatedConclusion() {
        val module = ModuleTesting("autoTransposition.")
        val transposed =
            module
                .value("transposition::transpose($rules, $rules, [], T)", "T")!!
                .let { Term.parse(it, module.solver.operators) }
                .castToList()
                .toList()
                .toSet()
        assertEquals(
            setOf("[s1, (a, b), c]", "[s1_i, ('-'(c), b), '-'(a)]", "[s1_ii, ('-'(c), a), '-'(b)]").map { Term.parse(it) }.toSet(),
            transposed,
        )
    }

    // c and d conflict both ways, contradictories as c and -c; a conflicts with e one way only, which is no contradictory
    @Test
    fun aConflictDeclaredBothWaysIsTransposedAsANegation() {
        val module = ModuleTesting("autoTransposition.\nconflict([c], [d]).\nconflict([d], [c]).\nconflict([a], [e]).")
        val transposed =
            module
                .value("transposition::transpose($rules, $rules, [], T)", "T")!!
                .let { Term.parse(it, module.solver.operators) }
                .castToList()
                .toList()
                .toSet()
        val expected =
            setOf(
                "[s1, (a, b), c]",
                "[s1_i, ('-'(c), b), '-'(a)]",
                "[s1_ii, ('-'(c), a), '-'(b)]",
                "[transposed(s1_i, [d, '-'(a)]), (d, b), '-'(a)]",
                "[transposed(s1_ii, [d, '-'(b)]), (d, a), '-'(b)]",
            )
        assertEquals(expected.map { Term.parse(it) }.toSet(), transposed)
    }

    // without the declared conflicts in the conflict model, c and d are no contradictories: only the negations count
    @Test
    fun aConflictDeclaredOutOfTheConflictModelIsNotTransposed() {
        val module = ModuleTesting("autoTransposition.\nconflictModel(negation).\nconflict([c], [d]).\nconflict([d], [c]).")
        val transposed =
            module
                .value("transposition::transpose($rules, $rules, [], T)", "T")!!
                .let { Term.parse(it, module.solver.operators) }
                .castToList()
                .toList()
                .toSet()
        assertEquals(
            setOf("[s1, (a, b), c]", "[s1_i, ('-'(c), b), '-'(a)]", "[s1_ii, ('-'(c), a), '-'(b)]").map { Term.parse(it) }.toSet(),
            transposed,
        )
    }

    // d, an axiom, conflicts with c both ways: c rests strictly on the defeasible a, so -a follows from d strictly,
    // and a is out with c, as consistency wants
    @Test
    fun theDeclaredContradictoriesKeepTheConclusionsConsistent() {
        val theory = "r1 : [] => a.\ns1 : a -> c.\nf1 :-> d.\nconflict([c], [d]).\nconflict([d], [c]).\n"
        val solver = Arg2pSolverFactory.default(theory = theory, settings = FlagsBuilder().create())
        val solution = solver.solve(Struct.parse("buildLabelSets(I, O, _)", solver.operators)).first()
        assertEquals("[['-'(a)], [d]]", solution.substitution.getByName("I").toString())
        assertEquals("[[c], [a]]", solution.substitution.getByName("O").toString())
    }

    // a rule of the theory is named s1_i: the transposition takes another name, rather than the place of the rule
    @Test
    fun aTranspositionNeverTakesTheNameOfARuleOfTheTheory() {
        val module = ModuleTesting("autoTransposition.")
        val transposed =
            module
                .value("transposition::transpose($rules, $rules, [s1_i], T)", "T")!!
                .let { Term.parse(it, module.solver.operators) }
                .castToList()
                .toList()
                .toSet()
        assertEquals(
            setOf("[s1, (a, b), c]", "[transposition(s1, '_i'), ('-'(c), b), '-'(a)]", "[s1_ii, ('-'(c), a), '-'(b)]")
                .map { Term.parse(it) }
                .toSet(),
            transposed,
        )
    }

    // s_i, a strict rule of the theory, keeps concluding e beside the transpositions of s
    @Test
    fun aRuleNamedAsATranspositionKeepsItsArguments() {
        val theory = "s : a, b -> c.\ns_i : d -> e.\nf1 :=> a.\nf2 :=> b.\nf3 :=> d.\n"
        val solver = Arg2pSolverFactory.default(theory = theory, settings = FlagsBuilder().create())
        val solution = solver.solve(Struct.parse("buildLabelSets(I, _, _)", solver.operators)).first()
        assertTrue("e" in solution.substitution.getByName("I").toString())
    }

    @Test
    fun withoutAutoTranspositionTheRulesStayAsTheyAre() {
        // on in the profile aspic_plus, the default
        val module = ModuleTesting("autoTransposition(off).")
        assertEquals(Term.parse(rules), Term.parse(module.value("transposition::transpose($rules, $rules, [], T)", "T")!!))
    }

    // Matched as they are: -X, the permissions p/1 and nothing else
    @Test
    fun negations() {
        val module = ModuleTesting()
        assertEquals(
            listOf("a", "-(p)", "o(-(a))", "o(a)", "-(p(a, b))", "-(a - b)").map { Term.parse(it) },
            listOf("-(a)", "p", "p(a)", "p(-(a))", "p(a, b)", "a - b").map {
                Term.parse(module.value("transposition::negate($it, N)", "N")!!)
            },
        )
    }
}
