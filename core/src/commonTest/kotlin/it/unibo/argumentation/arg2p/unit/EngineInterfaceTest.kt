package it.unibo.argumentation.arg2p.unit

import it.unibo.argumentation.arg2p.TestingUtils
import it.unibo.argumentation.arg2p.TestingUtils.testGoalNoBacktracking
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.dsl.prolog
import it.unibo.tuprolog.solve.yes
import kotlin.test.Test

class EngineInterfaceTest {

    private fun solverWithTheory() = TestingUtils.solver(
        TestingUtils.withArgOperators(
            """
                r0 : -c => a.
                r1 : a => c.
                r2 : [] => -a.
                r3 : [] => -c.
                
                bp(-a).
            """))

    @Test
    fun buildLabelSets() {
        prolog {
            val solver = solverWithTheory()
            solver.solve(Struct.parse("disableBPCompletion")).toList()
            testGoalNoBacktracking("buildLabelSets"(
                listOf("StatIn", "StatOut", "StatUnd"), listOf("ArgIn", "ArgOut", "ArgUnd")), solver) {
                    it.yes(
                        "StatIn" to Struct.parse("""
                                [
                                    [a]
                                ]"""),
                        "StatOut" to Struct.parse("""
                                [
                                    [neg,a]
                                ]"""),
                        "StatUnd" to Struct.parse("""
                                [
                                    [neg, c],
                                    [c]
                                ]"""),
                        "ArgIn" to Struct.parse("""
                                [
                                    [[r3,r0],r0,[a]]
                                ]"""),
                        "ArgOut" to Struct.parse("""
                                [
                                    [[r2],r2,[neg,a]]
                                ]"""),
                        "ArgUnd" to Struct.parse("""
                                [
                                    [[r3],r3,[neg,c]],
                                    [[r3,r1,r0],r1,[c]]
                                ]""")
                )
            }
        }
    }

    @Test
    fun answerQuery() {
        prolog {
            testGoalNoBacktracking("answerQuery"("a", "Y", "O", "U"), solverWithTheory()) {
                it.yes(
                    "Y" to listOf("a"),
                    "O" to emptyList,
                    "U" to emptyList
                )
            }

            testGoalNoBacktracking("answerQuery"("c", "Y", "O", "U"), solverWithTheory()) {
                it.yes(
                    "Y" to emptyList,
                    "O" to listOf("c"),
                    "U" to emptyList
                )
            }
        }
    }
}