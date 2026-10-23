package it.unibo.tuprolog.argumentation.core.modules.query

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
}
