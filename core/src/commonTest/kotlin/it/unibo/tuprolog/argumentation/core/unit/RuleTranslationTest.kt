package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.TestingUtils.solver
import it.unibo.tuprolog.argumentation.core.TestingUtils.testYesGoal
import it.unibo.tuprolog.argumentation.core.TestingUtils.withArgOperators
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.dsl.logicProgramming
import kotlin.test.Ignore
import kotlin.test.Test

@Ignore
class RuleTranslationTest {

    private fun testTranslation(theory: String, vararg expected: String) {
        logicProgramming {
            val arg2p = Arg2pSolver.default()
            val solver = solver(withArgOperators(theory))
            solver.solve(Struct.parse("context_reset", arg2p.operators())).first()
            solver.solve(Struct.parse("parser::convertAllRules(_)", arg2p.operators())).toList()
            expected.forEach { testYesGoal(Struct.parse("context_check($it)"), solver) }
        }
    }

    @Test
    fun defeasibleRuleWithEmptyPremises() =
        testTranslation("r0 : [] => a.", "rule([r0, [], [a]])")

    @Test
    fun defeasibleRuleWithEmptyPremisesAndNegation() =
        testTranslation("r0 : [] => -a.", "rule([r0, [], [neg, a]])")

    @Test
    fun defeasibleRuleWithEmptyPremisesAndObligation() =
        testTranslation("r0 : [] => o(-a).", "rule([r0, [], [obl, [neg, a]]])")

    @Test
    fun defeasibleRuleWithEmptyPremisesAndPermission() =
        testTranslation("r0 : [] => p(-a).", "rule([r0, [], [perm, [neg, a]]])")

    @Test
    fun defeasibleRule() =
        testTranslation("r0 : a => o(-b).", "rule([r0, [[a]], [obl, [neg, b]]])")

    @Test
    fun defeasibleRuleWithVar() =
        testTranslation(
            "r0 : a(X), o(b(X)) => -c(X).",
            "rule([r0, [[a(_)],[obl,[b(_)]]], [neg,c(_)]])"
        )

    @Test
    fun strictRuleWithEmptyPremises() =
        testTranslation("r0 : [] -> a.", "rule([r0, [], [a]])", "strict(r0)")

    @Test
    fun strictRuleWithEmptyPremisesAndNegation() =
        testTranslation("r0 : [] -> -a.", "rule([r0, [], [neg, a]])", "strict(r0)")

    @Test
    fun strictRuleWithEmptyPremisesAndObligation() =
        testTranslation("r0 : [] -> o(-a).", "rule([r0, [], [obl, [neg, a]]])", "strict(r0)")

    @Test
    fun strictRuleWithEmptyPremisesAndPermission() =
        testTranslation("r0 : [] -> p(-a).", "rule([r0, [], [perm, [neg, a]]])", "strict(r0)")

    @Test
    fun strictRule() =
        testTranslation("r0 : a -> o(-b).", "rule([r0, [[a]], [obl, [neg, b]]])", "strict(r0)")

    @Test
    fun strictRuleWithVar() =
        testTranslation(
            "r0 : a(X), o(b(X)) -> -c(X).",
            "rule([r0, [[a(_)],[obl,[b(_)]]], [neg,c(_)]])",
            "strict(r0)"
        )

    @Test
    fun ordinaryPremise() =
        testTranslation("r0 :=> a.", "premise([r0, [a]])")

    @Test
    fun ordinaryPremiseAndNegation() =
        testTranslation("r0 :=> -a.", "premise([r0, [neg, a]])")

    @Test
    fun ordinaryPremiseAndObligation() =
        testTranslation("r0 :=> o(-a).", "premise([r0, [obl, [neg, a]]])")

    @Test
    fun ordinaryPremiseAndPermission() =
        testTranslation("r0 :=> p(-a).", "premise([r0, [perm, [neg, a]]])")

    @Test
    fun ordinaryPremiseWithVar() =
        testTranslation("r0 :=> -c(X).", "premise([r0, [neg,c(_)]])")

    @Test
    fun axiomPremise() =
        testTranslation("r0 :-> a.", "premise([r0, [a]])", "strict(r0)")

    @Test
    fun axiomPremiseAndNegation() =
        testTranslation("r0 :-> -a.", "premise([r0, [neg, a]])", "strict(r0)")

    @Test
    fun axiomPremiseAndObligation() =
        testTranslation("r0 :-> o(-a).", "premise([r0, [obl, [neg, a]]])", "strict(r0)")

    @Test
    fun axiomPremiseAndPermission() =
        testTranslation("r0 :-> p(-a).", "premise([r0, [perm, [neg, a]]])", "strict(r0)")

    @Test
    fun axiomPremiseWithVar() =
        testTranslation("r0 :-> -c(X).", "premise([r0, [neg,c(_)]])", "strict(r0)")

    @Test
    fun baseBp() =
        testTranslation("bp(a).", "abstractBp([[a]])")

    @Test
    fun bpWithVar() =
        testTranslation("bp(-a(X)).", "abstractBp([[neg, a(_)]])")

    @Test
    fun multivaluedBp() =
        testTranslation(
            "bp(-a(X), o(b(X)), p(c)).",
            "abstractBp([[neg, a(_)], [obl, [b(_)]], [perm, [c]]])"
        )

    @Test
    fun undercutOnPremise() =
        testTranslation("r0 :=> undercut(rule).", "premise([r0, [undercut(rule)]])")

    @Test
    fun undercutOnRule() =
        testTranslation("r0 : a => undercut(rule).", "rule([r0, [[a]], [undercut(rule)]])")

    @Test
    fun weakNegation() =
        testTranslation(
            "r0 : a, ~(b) => undercut(rule).",
            "rule([r0, [[a], [unless, [b]]], [undercut(rule)]])"
        )

    @Test
    fun prologAntecedent() =
        testTranslation(
            "r0 : a, ~(b), prolog(a == a) => undercut(rule).",
            "rule([r0, [[a], [unless, [b]], [prolog(a == a)]], [undercut(rule)]])"
        )

    @Test
    fun autoTransposition() =
        testTranslation(
            """
            r0 : a -> b.
            autoTransposition.
            """.trimIndent(),
            "rule([r0, [[a]], [b]])",
            "strict(r0)",
            "rule([r0_i, [[neg, b]], [neg, a]])",
            "strict(r0_i)"
        )

    @Test
    fun autoTranspositionMulti() =
        testTranslation(
            """
            r0 : a, b -> c.
            autoTransposition.
            """.trimIndent(),
            "rule([r0, [[a], [b]], [c]])",
            "strict(r0)",
            "rule([r0_i, [[neg, c], [b]], [neg, a]])",
            "strict(r0_i)",
            "rule([r0_ii, [[neg, c], [a]], [neg, b]])",
            "strict(r0_ii)"
        )
}
