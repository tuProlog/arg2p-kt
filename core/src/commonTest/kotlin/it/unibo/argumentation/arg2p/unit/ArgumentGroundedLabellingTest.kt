package it.unibo.argumentation.arg2p.unit

import it.unibo.argumentation.arg2p.TestingUtils.solver
import it.unibo.argumentation.arg2p.TestingUtils.testGoalNoBacktracking
import it.unibo.argumentation.arg2p.TestingUtils.withArgOperators
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.dsl.prolog
import it.unibo.tuprolog.solve.yes
import kotlin.test.Test


class ArgumentGroundedLabellingTest {

    private fun solverWithTheory() = solver(
        withArgOperators("""
            attack([[r2,r0],r2,[neg,r('Pippo')]],[[r3,r1],r3,[r('Pippo')]]).
            attack([[r2,r0],r2,[neg,r('Pippo')]],[[r4,r3,r1],r4,[s('Pippo')]]).
            attack([[r4,r3,r1],r4,[s('Pippo')]],[[r5],r5,[neg,s('Pippo')]]).
            attack([[r5],r5,[neg,s('Pippo')]],[[r4,r3,r1],r4,[s('Pippo')]]).
        """)
    )

    private fun argumentationGraph() =
        Struct.parse("""
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
                    ([[r2,r0],r2,[neg,r('Pippo')]],[[r3,r1],r3,[r('Pippo')]]),
                    ([[r2,r0],r2,[neg,r('Pippo')]],[[r4,r3,r1],r4,[s('Pippo')]]),
                    ([[r4,r3,r1],r4,[s('Pippo')]],[[r5],r5,[neg,s('Pippo')]]),
                    ([[r5],r5,[neg,s('Pippo')]],[[r4,r3,r1],r4,[s('Pippo')]])
                ],
                [
                    ([[r3,r1],r3,[r('Pippo')]],[[r4,r3,r1],r4,[s('Pippo')]]),
                    ([[r1],r1,[q('Pippo')]],[[r3,r1],r3,[r('Pippo')]]),
                    ([[r0],r0,[a('Pippo')]],[[r2,r0],r2,[neg,r('Pippo')]])
                ]
            ]
        """)

    @Test
    fun labelArguments() {
        prolog {
            testGoalNoBacktracking("argumentLabelling"(argumentationGraph(), listOf("IN", "OUT", "UND")),
                solverWithTheory()) {
                    it.yes(
                        "IN" to Struct.parse("""
                            [
                                [[r2,r0],r2,[neg,r('Pippo')]],
                                [[r1],r1,[q('Pippo')]],
                                [[r0],r0,[a('Pippo')]],
                                [[r5],r5,[neg,s('Pippo')]]
                            ]"""),
                        "OUT" to Struct.parse("""
                            [
                                [[r4,r3,r1],r4,[s('Pippo')]],
                                [[r3,r1],r3,[r('Pippo')]]
                            ]"""),
                        "UND" to emptyList
                    )
            }
        }
    }
}