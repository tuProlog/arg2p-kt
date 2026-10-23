package it.unibo.tuprolog.argumentation.core.modules.graph.partial

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class QueryExplorationModuleTest {
    // b depends on a, d on c, and -a attacks a
    private val theory = "r1 : a => b.\nr2 : [] => a.\nr3 : c => d.\nr4 : [] => c.\nr5 : [] => -a.\nqueryMode."

    private fun module() =
        ModuleTesting(theory).also {
            it.run("context_reset", "parser:::convertAllRules(_)")
        }

    private fun ModuleTesting.claims() = values("context_check(argument(A)), argument::claim(A, C)", "C").toSet()

    @Test
    fun aQueryExploresTheArgumentsItDependsOn() {
        val module = module()
        module.run("queryExploration::explore(b)")
        // the argument for b, its sub-argument for a, and the attacker of a; nothing for c and d
        assertEquals(setOf("b", "a", "'-'(a)"), module.claims())
        assertTrue(module.holds("graphStore::argumentFor([b], B), graphStore::argumentFor([a], A), context_check(support(A, B))"))
    }

    @Test
    fun anArgumentIsFoundFromItsRulesAndPremises() {
        val module = module()
        val argument = module.value("argumentSearch::findSingleArgument([b], A, _)", "A")!!
        assertTrue(module.holds("A = $argument, argument::topRule(A, r1), argument::claim(A, b), argument::body(A, [a])"))
        assertTrue(module.holds("A = $argument, argument::rules(A, R), member(r1, R), member(r2, R)"))
        assertTrue(module.holds("A = $argument, argument::info(A, [[r1], D, []]), member(r1, D), member(r2, D)"))
        // its only sub-argument is the one for a
        assertTrue(module.holds("argumentSearch::findSingleArgument([b], _, [S]), argument::claim(S, a)"))
    }

    @Test
    fun aStatementIsExploredOnce() {
        val module = module()
        module.run("queryExploration::explore(b)")
        val saved = module.solutions("context_check(argument(_))").count { it.isYes }
        module.run("queryExploration::explore(b)")
        assertEquals(saved, module.solutions("context_check(argument(_))").count { it.isYes })
    }

    // The arguments are the same terms as those of the whole graph: rules and info sorted, the body as written
    @Test
    fun theArgumentsAreThoseOfTheWholeGraph() {
        val theory =
            "f1 :=> a.\nf2 :=> b.\nr1 : b, a, prolog(true) => c.\nr2 : c, a => d.\nr3 : d -> e.\n" +
                "graphBuildMode(standard_af).\nstatementLabellingMode(statement).\nargumentLabellingMode(grounded).\n"
        val arguments = "context_check(argument(A))"
        val whole = ModuleTesting(theory).also { it.run("buildLabelSets(_, _, _)") }
        val query = ModuleTesting(theory + "queryMode.").also { it.run("answerQuery(e, _, _, _)") }
        assertEquals(whole.values(arguments, "A").toSet(), query.values(arguments, "A").toSet())
    }

    // p(a, b) is not covered by the exploration of p(V, V) (for q): t(a, b) is found
    @Test
    fun aMoreGeneralStatementIsExploredAgain() {
        val module =
            ModuleTesting(
                "f1 :=> p(a, a).\nf2 :=> p(a, b).\nr1 : p(X, X) => q(X).\nr2 : p(X, Y) => s(X, Y).\n" +
                    "r3 : q(_), s(A, B) => t(A, B).\nqueryMode.\ngraphBuildMode(standard_af).\n" +
                    "statementLabellingMode(statement).\nargumentLabellingMode(grounded).",
            )
        val statIn = module.value("answerQuery(t(X, Y), I, _, _)", "I")!!
        assertTrue("t(a, a)" in statIn && "t(a, b)" in statIn, statIn)
    }
}
