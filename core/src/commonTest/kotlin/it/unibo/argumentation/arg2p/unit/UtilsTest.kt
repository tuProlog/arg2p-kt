package it.unibo.argumentation.arg2p.unit

import it.unibo.argumentation.arg2p.Arg2p
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.dsl.prolog
import it.unibo.tuprolog.solve.*
import it.unibo.tuprolog.solve.library.Libraries
import kotlin.test.Test
import it.unibo.tuprolog.theory.Theory
import kotlin.test.assertEquals
import kotlin.collections.listOf as ktListOf

class UtilsTest {

    private fun solver(theory : Theory = Theory.empty()) =
        ClassicSolverFactory.solverWithDefaultBuiltins(
            otherLibraries = Libraries(Arg2p),
            staticKb = theory
        )

    private fun testGoal(goal: Struct, solver : Solver = solver(), expectedSolutions: (Struct) -> Iterable<Solution>) {
        val solutions = solver.solve(goal).toList()
        assertSolutionEquals(
            expectedSolutions(goal),
            solutions
        )
    }

    @Test
    fun assertaList() {
        prolog {
            val solver = solver()
            assertEquals(0, solver.dynamicKb.size)
            solver.solve("assertaList"(listOf("a", "b"))).toList()
            assertEquals(2, solver.dynamicKb.size)
        }
    }

    @Test
    fun sort() {
        prolog {
            testGoal("sort"(listOf("b", "a", "c", "d"), "X")) {
                ktListOf(
                    it.yes("X" to listOf("d", "c", "b", "a")),
                    it.no()
                )
            }
        }
    }

    @Test
    fun subtractList() {
        prolog {
            testGoal("subtract"(listOf("b", "a", "c", "d", "a"), listOf("b", "a"), "X")) {
                ktListOf(
                    it.yes("X" to listOf("c", "d"))
                )
            }
        }
    }

    @Test
    fun isEmptyList() {
        prolog {
            testGoal("isEmptyList"(emptyList)) {
                ktListOf(it.yes())
            }

            testGoal("isEmptyList"(listOf("a"))) {
                ktListOf(it.no())
            }
        }
    }

    @Test
    fun appendaList() {
        prolog {
            testGoal("appendLists"(listOf(listOf("a", "b"), listOf("c", "d"), listOf("e", "f")), "X")) {
                ktListOf(
                    it.yes("X" to listOf("a", "b", "c", "d", "e", "f"))
                )
            }

            testGoal("appendLists"(listOf(listOf("a", "b")), "X")) {
                ktListOf(
                    it.yes("X" to listOf("a", "b"))
                )
            }
        }
    }

    @Test
    fun search() {
        prolog {

            val solver = solver(Theory.Companion.of(ktListOf(
                fact { "a"(1) },
                fact { "a"(1, 2) },
                fact { "a"(1, 2, 3) },
                fact { "a"(1, 2, 3, 4) })))

            testGoal("search"("a", 4, "X"), solver) {
                ktListOf(
                    it.yes("X" to "a"(1)),
                    it.yes("X" to "a"(1, 2)),
                    it.yes("X" to "a"(1, 2, 3)),
                    it.yes("X" to "a"(1, 2, 3, 4))
                )
            }

            testGoal("search"("a", 2, "X"), solver) {
                ktListOf(
                    it.yes("X" to "a"(1)),
                    it.yes("X" to "a"(1, 2))
                )
            }

            testGoal("search"("b", 2, "X"), solver) {
                ktListOf(it.no())
            }
        }
    }
}