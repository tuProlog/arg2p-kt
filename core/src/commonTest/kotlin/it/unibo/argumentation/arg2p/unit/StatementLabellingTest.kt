package it.unibo.argumentation.arg2p.unit

import it.unibo.argumentation.arg2p.TestingUtils.testGoalNoBacktracking
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.dsl.prolog
import it.unibo.tuprolog.solve.yes
import kotlin.test.Test

class StatementLabellingTest {

    private fun argumentLabelling() =
        Struct.parse(
            """
            [
                [
                    [[r3,r0],r0,[a]],
                    [[r3],r3,[neg,c]]
                ],
                [
                    [[r2],r2,[neg,a]],
                    [[r3,r1,r0],r1,[c]]
                ],
                []
            ]
        """
        )

    @Test
    fun labelStatements() {
        prolog {
            testGoalNoBacktracking("statementLabelling"(argumentLabelling(), listOf("In", "Out", "Und"))) {
                it.yes(
                    "In" to Struct.parse(
                        """
                            [
                                [a],
                                [neg,c]
                            ]"""
                    ),
                    "Out" to Struct.parse(
                        """
                            [
                                [neg,a],
                                [c]
                            ]"""
                    ),
                    "Und" to emptyList
                )
            }
        }
    }
}
