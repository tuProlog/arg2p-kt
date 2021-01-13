package it.unibo.argumentation.arg2p.unit

import it.unibo.argumentation.arg2p.Arg2p
import it.unibo.argumentation.arg2p.TestingUtils
import it.unibo.argumentation.arg2p.TestingUtils.testGoalNoBacktracking
import it.unibo.argumentation.arg2p.TestingUtils.testYesGoal
import it.unibo.argumentation.arg2p.TestingUtils.withArgOperators
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.dsl.prolog
import it.unibo.tuprolog.solve.classic.ClassicSolverFactory
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.solve.library.Libraries
import it.unibo.tuprolog.solve.yes
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.collections.listOf as ktListOf

class ArgumentationGraphTest {

    private fun solverWithTheory() = TestingUtils.solver(
        withArgOperators("""
                    rule([r0,[[neg,c]],[a]]).
                    rule([r1,[[a]],[c]]).
                    rule([r2,[],[neg,a]]).
                    rule([r3,[],[neg,c]]).
                    
                    orderingPrinciple(last).
                    orderingComparator(democrat).
                """)
    )

    @Test
    fun buildArgumentationGraph() {
        prolog {
            val solver = solverWithTheory()
            testGoalNoBacktracking("buildArgumentationGraph"(listOf("Arguments", "Attacks", "Supports")), solver) {
                it.yes(
                    "Arguments" to Struct.parse("""
                        [
                            [[r3,r1,r0],r1,[c]],
                            [[r3,r0],r0,[a]],
                            [[r3],r3,[neg,c]],
                            [[r2],r2,[neg,a]]
                        ]"""),
                    "Attacks" to Struct.parse("""
                        [
                            (rebut,[[r3,r1,r0],r1,[c]],[[r3,r1,r0],r1,[c]]),
                            (rebut,[[r3,r1,r0],r1,[c]],[[r3,r0],r0,[a]]),
                            (rebut,[[r2],r2,[neg,a]],[[r3,r1,r0],r1,[c]]),
                            (rebut,[[r2],r2,[neg,a]],[[r3,r0],r0,[a]]),
                            (rebut,[[r3],r3,[neg,c]],[[r3,r1,r0],r1,[c]]),
                            (rebut,[[r3,r0],r0,[a]],[[r2],r2,[neg,a]]),
                            (rebut,[[r3,r1,r0],r1,[c]],[[r3],r3,[neg,c]])
                        ]"""),
                    "Supports" to Struct.parse("""
                        [
                            ([[r3,r0],r0,[a]],[[r3,r1,r0],r1,[c]]),
                            ([[r3],r3,[neg,c]],[[r3,r0],r0,[a]])
                        ]"""),
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
            assertEquals(13, solver.dynamicKb.size)

            testYesGoal(ktListOf(
                Struct.parse("attack(rebut,[[r3,r1,r0],r1,[c]],[[r3,r1,r0],r1,[c]])"),
                Struct.parse("attack(rebut,[[r2],r2,[neg,a]],[[r3,r0],r0,[a]])"),
                Struct.parse("attack(rebut,[[r2],r2,[neg,a]],[[r3,r1,r0],r1,[c]])"),
                Struct.parse("attack(rebut,[[r3],r3,[neg,c]],[[r3,r1,r0],r1,[c]])"),
                Struct.parse("attack(rebut,[[r3,r0],r0,[a]],[[r2],r2,[neg,a]])"),
                Struct.parse("attack(rebut,[[r3,r1,r0],r1,[c]],[[r3],r3,[neg,c]])"),
                Struct.parse("attack(rebut,[[r3,r1,r0],r1,[c]],[[r3,r0],r0,[a]])")
            ), solver)
        }
    }

//    @Test
//    fun test() {
//        prolog {
//            val solver = ClassicSolverFactory.solverWithDefaultBuiltins(
//                otherLibraries = Libraries.of(Arg2p),
//                dynamicKb = Theory.parse("""
//                    argument([[r3,r1,r0],r1,[c]]).
//                    argument([[r3,r0],r0,[a]]).
//                    argument([[r3],r3,[neg,c]]).
//                    argument([[r2],r2,[neg,a]]).
//                    support([[r3,r0],r0,[a]],[[r3,r1,r0],r1,[c]]).
//                    support([[r3],r3,[neg,c]],[[r3,r0],r0,[a]]).
//                """.trimIndent()),
////                staticKb = Theory.parse("""
////                    prova :- asserta(a), prova2, fail.
////                    prova :- asserta(c).
////                    prova2 :- asserta(b), fail.
////                    prova2.
////                """.trimIndent())
//            )
//
//            solver.solve(Struct.parse("buildAttacks")).toList()
////            solver.solve(Struct.parse("prova")).toList()
//            assertEquals(2, solver.dynamicKb.size)
//        }
//    }

    @Test
    fun buildArguments() {
        prolog {
            val solver = solverWithTheory()
            solver.solve(Struct.parse("buildArguments")).toList()
            assertEquals(6, solver.dynamicKb.size)

            testYesGoal(ktListOf(
                Struct.parse("argument([[r3,r1,r0],r1,[c]])"),
                Struct.parse("argument([[r3,r0],r0,[a]])"),
                Struct.parse("argument([[r3],r3,[neg,c]])"),
                Struct.parse("argument([[r2],r2,[neg,a]])"),
                Struct.parse("support([[r3,r0],r0,[a]],[[r3,r1,r0],r1,[c]])"),
                Struct.parse("support([[r3],r3,[neg,c]],[[r3,r0],r0,[a]])")
            ), solver)
        }
    }
}