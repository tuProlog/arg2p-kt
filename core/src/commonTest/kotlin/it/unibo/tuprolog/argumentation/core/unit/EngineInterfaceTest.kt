package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.TestingUtils
import it.unibo.tuprolog.argumentation.core.TestingUtils.testGoalNoBacktracking
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.dsl.logicProgramming
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
                
                graphBuildMode(standard_af).
                statementLabellingMode(statement).
                argumentLabellingMode(bp_grounded_partial).
                graphExtension(rebutRestriction).
                graphExtension(standardPref).
                orderingPrinciple(last).
                orderingComparator(democrat).
            """
        )
    )

    @Test
    fun buildLabelSets() {
        logicProgramming {
            val solver = solverWithTheory()
            testGoalNoBacktracking(
                "buildLabelSets"(
                    listOf("StatIn", "StatOut", "StatUnd"),
                    listOf("ArgIn", "ArgOut", "ArgUnd")
                ),
                solver
            ) {
                it.yes(
                    "StatIn" to Struct.parse(
                        """
                                [
                                    [a]
                                ]"""
                    ),
                    "StatOut" to Struct.parse(
                        """
                                [
                                    [-a]
                                ]"""
                    ),
                    "StatUnd" to Struct.parse(
                        """
                                [
                                    [-c],
                                    [c]
                                ]"""
                    ),
                    "ArgIn" to Struct.parse(
                        """
                                [
                                    [[r3,r0],r0,[a],[-c],[[r0],[r3,r0],[]]]
                                ]"""
                    ),
                    "ArgOut" to Struct.parse(
                        """
                                [
                                    [[r2],r2,[-a],[],[[r2],[r2],[]]]
                                ]"""
                    ),
                    "ArgUnd" to Struct.parse(
                        """
                                [
                                    [[r3,r1,r0],r1,[c],[a],[[r1],[r3,r1,r0],[]]],
                                    [[r3],r3,[-c],[],[[r3],[r3],[]]]
                                ]"""
                    )
                )
            }
        }
    }

    @Test
    fun answerQuery() {
        logicProgramming {
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
                    "O" to emptyList,
                    "U" to listOf("c")
                )
            }
        }
    }
}
