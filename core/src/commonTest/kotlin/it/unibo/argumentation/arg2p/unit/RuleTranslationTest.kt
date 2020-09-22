package it.unibo.argumentation.arg2p.unit

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.dsl.prolog
import kotlin.test.Test
import it.unibo.argumentation.arg2p.TestingUtils.solver
import it.unibo.argumentation.arg2p.TestingUtils.testYesGoal
import it.unibo.argumentation.arg2p.TestingUtils.withArgOperators


class RuleTranslationTest {

    @Test
    fun baseFact() {
        prolog {
            val solver = solver(
                withArgOperators("""
                    r0 : [] => a.
                """)
            )

            solver.solve(Struct.parse("convertAllRules")).toList()
            testYesGoal("rule"(listOf("r0", emptyList, listOf("a"))), solver)
        }
    }

    @Test
    fun factWithNegation() {
        prolog {
            val solver = solver(
                withArgOperators("""
                    r0 : [] => -a.
                """)
            )

            solver.solve(Struct.parse("convertAllRules")).toList()
            testYesGoal("rule"(listOf("r0", emptyList, listOf("neg", "a"))), solver)
        }
    }

    @Test
    fun factWithObligation() {
        prolog {
            val solver = solver(
                withArgOperators("""
                    r0 : [] => o(-a).
                """)
            )

            solver.solve(Struct.parse("convertAllRules")).toList()
            testYesGoal("rule"(listOf("r0", emptyList, listOf("obl", listOf("neg", "a")))), solver)
        }
    }

    @Test
    fun factWithPermission() {
        prolog {
            val solver = solver(
                withArgOperators("""
                    r0 : [] => p(-a).
                """)
            )

            solver.solve(Struct.parse("convertAllRules")).toList()
            testYesGoal("rule"(listOf("r0", emptyList, listOf("perm", listOf("neg", "a")))), solver)
        }
    }

    @Test
    fun baseRule() {
        prolog {
            val solver = solver(
                withArgOperators("""
                    r0 : a => o(-b).
                """)
            )

            solver.solve(Struct.parse("convertAllRules")).toList()
            testYesGoal("rule"(listOf("r0", listOf(listOf("a")), listOf("obl", listOf("neg", "b")))), solver)
        }
    }

    @Test
    fun ruleWithVar() {
        prolog {
            val solver = solver(
                withArgOperators("""
                    r0 : a(X), o(b(X)) => -c(X).
                """)
            )

            solver.solve(Struct.parse("convertAllRules")).toList()
            testYesGoal(Struct.parse("rule([r0, [[a(_)],[obl,[b(_)]]], [neg,c(_)]])"), solver)
        }
    }

    @Test
    fun baseBp() {
        prolog {
            val solver = solver(
                withArgOperators("""
                    bp(a).
                """)
            )

            solver.solve(Struct.parse("convertAllRules")).toList()
            testYesGoal(Struct.parse("abstractBp([[a]])"), solver)
        }
    }

    @Test
    fun bpWithVar() {
        prolog {
            val solver = solver(
                withArgOperators("""
                    bp(-a(X)).
                """)
            )

            solver.solve(Struct.parse("convertAllRules")).toList()
            testYesGoal(Struct.parse("abstractBp([[neg, a(_)]])"), solver)
        }
    }

    @Test
    fun multivaluedBp() {
        prolog {
            val solver = solver(
                withArgOperators("""
                    bp(-a(X), o(b(X)), p(c)).
                """)
            )

            solver.solve(Struct.parse("convertAllRules")).toList()
            testYesGoal(Struct.parse("abstractBp([[neg, a(_)], [obl, [b(_)]], [perm, [c]]])"), solver)
        }
    }
}