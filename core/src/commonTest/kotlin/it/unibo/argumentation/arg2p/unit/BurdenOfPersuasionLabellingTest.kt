package it.unibo.argumentation.arg2p.unit

import it.unibo.argumentation.arg2p.TestingUtils
import it.unibo.argumentation.arg2p.TestingUtils.testGoalNoBacktracking
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.dsl.prolog
import it.unibo.tuprolog.solve.yes
import kotlin.test.Test

class BurdenOfPersuasionLabellingTest {

    private fun solverWithTheory() = TestingUtils.solver(
        TestingUtils.withArgOperators("""
                argument([[r3,r1,r0],r1,[c]]).
                argument([[r3,r0],r0,[a]]).
                argument([[r3],r3,[neg,c]]).
                argument([[r2],r2,[neg,a]]).
                support([[r3,r0],r0,[a]],[[r3,r1,r0],r1,[c]]).
                support([[r3],r3,[neg,c]],[[r3,r0],r0,[a]]).
                attack([[r3,r1,r0],r1,[c]],[[r3,r1,r0],r1,[c]]).
                attack([[r2],r2,[neg,a]],[[r3,r0],r0,[a]]).
                attack([[r2],r2,[neg,a]],[[r3,r1,r0],r1,[c]]).
                attack([[r3],r3,[neg,c]],[[r3,r1,r0],r1,[c]]).
                attack([[r3,r0],r0,[a]],[[r2],r2,[neg,a]]).
                attack([[r3,r1,r0],r1,[c]],[[r3],r3,[neg,c]]).
                attack([[r3,r1,r0],r1,[c]],[[r3,r0],r0,[a]]).
                
                abstractBp([[neg, a]]).
            """))

    private fun groundedLabelling() =
        Struct.parse("""
            [
                [],
                [],
                [
                    [[r2],r2,[neg,a]],
                    [[r3,r0],r0,[a]],
                    [[r3,r1,r0],r1,[c]],
                    [[r3],r3,[neg,c]]
                ]
            ]
        """)

    @Test
    fun partialLabelling() {
        prolog {
            val solver = solverWithTheory()
            solver.solve(Struct.parse("disableBPCompletion")).toList()
            testGoalNoBacktracking("argumentBPLabelling"(groundedLabelling(), listOf("BPIN", "BPOUT", "BPUND")), solver) {
                it.yes(
                    "BPIN" to Struct.parse("""
                            [
                                [[r3,r0],r0,[a]]
                            ]"""),
                    "BPOUT" to Struct.parse("""
                            [
                                [[r2],r2,[neg,a]]
                            ]"""),
                    "BPUND" to Struct.parse("""
                            [
                                [[r3,r1,r0],r1,[c]],
                                [[r3],r3,[neg,c]]
                            ]""")
                )
            }
        }
    }

    @Test
    fun completeLabelling() {
        prolog {
            testGoalNoBacktracking("argumentBPLabelling"(groundedLabelling(), listOf("BPIN", "BPOUT", "BPUND")),
                solverWithTheory()) {
                    it.yes(
                        "BPIN" to Struct.parse("""
                                [
                                    [[r3,r0],r0,[a]],
                                    [[r3],r3,[neg,c]]
                                ]"""),
                        "BPOUT" to Struct.parse("""
                                [
                                    [[r2],r2,[neg,a]],
                                    [[r3,r1,r0],r1,[c]]
                                ]"""),
                        "BPUND" to emptyList
                    )
            }
        }
    }
}