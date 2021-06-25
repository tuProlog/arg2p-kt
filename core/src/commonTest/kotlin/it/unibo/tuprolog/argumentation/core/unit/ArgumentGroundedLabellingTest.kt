package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.TestingUtils.solver
import it.unibo.tuprolog.argumentation.core.TestingUtils.testGoalNoBacktracking
import it.unibo.tuprolog.argumentation.core.TestingUtils.withArgOperators
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.dsl.prolog
import it.unibo.tuprolog.solve.yes
import kotlin.test.Ignore
import kotlin.test.Test

class ArgumentGroundedLabellingTest {

    private fun solverWithTheory() = solver(
        withArgOperators(
            """
            attack(rebut,[[r2,r0],r2,[neg,r('Pippo')]],[[r3,r1],r3,[r('Pippo')]]).
            attack(rebut,[[r2,r0],r2,[neg,r('Pippo')]],[[r4,r3,r1],r4,[s('Pippo')]]).
            attack(rebut,[[r4,r3,r1],r4,[s('Pippo')]],[[r5],r5,[neg,s('Pippo')]]).
            attack(rebut,[[r5],r5,[neg,s('Pippo')]],[[r4,r3,r1],r4,[s('Pippo')]]).
        """
        )
    )

    private fun argumentationGraph() =
        Struct.parse(
            """
            [
                [
                    [[r5],r5,[neg,s('Pippo')]],
                    [[r4,r3,r1],r4,[s('Pippo')]],
                    [[r3,r1],r3,[r('Pippo')]],
                    [[r2,r0],r2,[neg,r('Pippo')]],
                    [[r1],r1,[q('Pippo')]],
                    [[r0],r0,[a('Pippo')]]
                ],
                [
                    (rebut,[[r2,r0],r2,[neg,r('Pippo')]],[[r3,r1],r3,[r('Pippo')]]),
                    (rebut,[[r2,r0],r2,[neg,r('Pippo')]],[[r4,r3,r1],r4,[s('Pippo')]]),
                    (rebut,[[r4,r3,r1],r4,[s('Pippo')]],[[r5],r5,[neg,s('Pippo')]]),
                    (rebut,[[r5],r5,[neg,s('Pippo')]],[[r4,r3,r1],r4,[s('Pippo')]])
                ],
                [
                    ([[r3,r1],r3,[r('Pippo')]],[[r4,r3,r1],r4,[s('Pippo')]]),
                    ([[r1],r1,[q('Pippo')]],[[r3,r1],r3,[r('Pippo')]]),
                    ([[r0],r0,[a('Pippo')]],[[r2,r0],r2,[neg,r('Pippo')]])
                ]
            ]
        """
        )

    @Test
    fun labelArguments() {
        prolog {
            testGoalNoBacktracking(
                "argumentGroundedLabelling"(argumentationGraph(), listOf("IN", "OUT", "UND")),
                solverWithTheory()
            ) {
                it.yes(
                    "IN" to Struct.parse(
                        """
                            [
                                [[r5],r5,[neg,s('Pippo')]],
                                [[r0],r0,[a('Pippo')]],
                                [[r1],r1,[q('Pippo')]],
                                [[r2,r0],r2,[neg,r('Pippo')]]
                            ]"""
                    ),
                    "OUT" to Struct.parse(
                        """
                            [
                                [[r3,r1],r3,[r('Pippo')]],
                                [[r4,r3,r1],r4,[s('Pippo')]]
                            ]"""
                    ),
                    "UND" to emptyList
                )
            }
        }
    }
}
