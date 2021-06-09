package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.TestingUtils
import it.unibo.tuprolog.argumentation.core.TestingUtils.testGoalNoBacktracking
import it.unibo.tuprolog.argumentation.core.TestingUtils.testYesGoal
import it.unibo.tuprolog.argumentation.core.TestingUtils.withArgOperators
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.dsl.prolog
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.solve.yes
import kotlin.test.Ignore
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.collections.listOf as ktListOf

@Ignore
class ArgumentationGraphTest {

    private fun solverWithTheory() = TestingUtils.solver(
        withArgOperators(
            """
                    rule([r0,[[neg,c]],[a]]).
                    rule([r1,[[a]],[c]]).
                    rule([r2,[],[neg,a]]).
                    rule([r3,[],[neg,c]]).
                    
                    orderingPrinciple(last).
                    orderingComparator(democrat).
                """
        )
    )

    @Test
    fun buildArgumentationGraph() {
        prolog {
            val solver = solverWithTheory()
            testGoalNoBacktracking("buildArgumentationGraph"(listOf("Arguments", "Attacks", "Supports")), solver) {
                it.yes(
                    "Arguments" to Struct.parse(
                        """
                        [
                            [[r3,r1,r0],r1,[c]],
                            [[r3,r0],r0,[a]],
                            [[r3],r3,[neg,c]],
                            [[r2],r2,[neg,a]]
                        ]"""
                    ),
                    "Attacks" to Struct.parse(
                        """
                        [
                            (rebut,[[r3,r1,r0],r1,[c]],[[r3],r3,[neg,c]]),
                            (rebut,[[r3,r0],r0,[a]],[[r2],r2,[neg,a]]),
                            (rebut,[[r3],r3,[neg,c]],[[r3,r1,r0],r1,[c]]),
                            (rebut,[[r2],r2,[neg,a]],[[r3,r0],r0,[a]]),
                            (rebut,[[r2],r2,[neg,a]],[[r3,r1,r0],r1,[c]]),
                            (rebut,[[r3,r1,r0],r1,[c]],[[r3,r0],r0,[a]]),
                            (rebut,[[r3,r1,r0],r1,[c]],[[r3,r1,r0],r1,[c]])
                        ]"""
                    ),
                    "Supports" to Struct.parse(
                        """
                        [
                            ([[r3,r0],r0,[a]],[[r3,r1,r0],r1,[c]]),
                            ([[r3],r3,[neg,c]],[[r3,r0],r0,[a]])
                        ]"""
                    ),
                )
            }
        }
    }

    @Test
    fun buildAttacks() {
        prolog {
            val solver = solverWithTheory()
            solver.setFlag(Unknown.name, Unknown.FAIL)
            solver.solve(Struct.parse("buildArguments")).toList()
            solver.solve(Struct.parse("buildAttacks")).toList()
            assertEquals(24, solver.dynamicKb.size)

            testYesGoal(
                ktListOf(
                    Struct.parse("attack(rebut,[[r3,r1,r0],r1,[c]],[[r3,r1,r0],r1,[c]])"),
                    Struct.parse("attack(rebut,[[r2],r2,[neg,a]],[[r3,r0],r0,[a]])"),
                    Struct.parse("attack(rebut,[[r2],r2,[neg,a]],[[r3,r1,r0],r1,[c]])"),
                    Struct.parse("attack(rebut,[[r3],r3,[neg,c]],[[r3,r1,r0],r1,[c]])"),
                    Struct.parse("attack(rebut,[[r3,r0],r0,[a]],[[r2],r2,[neg,a]])"),
                    Struct.parse("attack(rebut,[[r3,r1,r0],r1,[c]],[[r3],r3,[neg,c]])"),
                    Struct.parse("attack(rebut,[[r3,r1,r0],r1,[c]],[[r3,r0],r0,[a]])")
                ),
                solver
            )
        }
    }

    @Test
    fun buildArguments() {
        prolog {
            val solver = solverWithTheory()
            solver.solve(Struct.parse("buildArguments")).toList()
            assertEquals(10, solver.dynamicKb.size)

            testYesGoal(
                ktListOf(
                    Struct.parse("argument([[r3,r1,r0],r1,[c]])"),
                    Struct.parse("argument([[r3,r0],r0,[a]])"),
                    Struct.parse("argument([[r3],r3,[neg,c]])"),
                    Struct.parse("argument([[r2],r2,[neg,a]])"),
                    Struct.parse("support([[r3,r0],r0,[a]],[[r3,r1,r0],r1,[c]])"),
                    Struct.parse("support([[r3],r3,[neg,c]],[[r3,r0],r0,[a]])")
                ),
                solver
            )
        }
    }
}
