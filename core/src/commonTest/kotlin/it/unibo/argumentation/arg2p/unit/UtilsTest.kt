package it.unibo.argumentation.arg2p.unit

import it.unibo.argumentation.arg2p.Arg2p
import it.unibo.argumentation.arg2p.assertSolutionEquals
import it.unibo.argumentation.arg2p.no
import it.unibo.argumentation.arg2p.yes
import it.unibo.tuprolog.dsl.prolog
import it.unibo.tuprolog.solve.ClassicSolverFactory
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
            val solver = solver()
            val query = "sort"(listOf("b", "a", "c", "d"), "X")
            val solutions = solver.solve(query).toList()
            assertSolutionEquals(
                ktListOf(
                    query.yes("X" to listOf("d", "c", "b", "a")),
                    query.no()
                ),
                solutions
            )
        }
    }

    @Test
    fun subtractList() {
        prolog {
            val solver = solver()
            val query = "subtract"(listOf("b", "a", "c", "d", "a"), listOf("b", "a"), "X")
            val solutions = solver.solve(query).toList()
            assertSolutionEquals(
                ktListOf(
                    query.yes("X" to listOf("c", "d"))
                ),
                solutions
            )
        }
    }

    @Test
    fun isEmptyList() {
        prolog {
            val solver = solver()
            var query = "isEmptyList"(emptyList)
            var solutions = solver.solve(query).toList()
            assertSolutionEquals(
                ktListOf(query.yes()),
                solutions
            )

            query = "isEmptyList"(listOf("a"))
            solutions = solver.solve(query).toList()
            assertSolutionEquals(
                ktListOf(query.no()),
                solutions
            )
        }
    }

    @Test
    fun appendaList() {
        prolog {
            val solver = solver()

            var query = "appendLists"(listOf(
                listOf("a", "b"),
                listOf("c", "d"),
                listOf("e", "f")
            ), "X")
            var solutions = solver.solve(query).toList()
            assertSolutionEquals(
                ktListOf(
                    query.yes("X" to listOf("a", "b", "c", "d", "e", "f"))
                ),
                solutions
            )

            query = "appendLists"(listOf(
                listOf("a", "b")
            ), "X")
            solutions = solver.solve(query).toList()
            assertSolutionEquals(
                ktListOf(
                    query.yes("X" to listOf("a", "b"))
                ),
                solutions
            )
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

            var query = "search"("a", 4,"X")
            var solutions = solver.solve(query).toList()
            assertSolutionEquals(
                ktListOf(
                    query.yes("X" to "a"(1)),
                    query.yes("X" to "a"(1, 2)),
                    query.yes("X" to "a"(1, 2, 3)),
                    query.yes("X" to "a"(1, 2, 3, 4))
                ),
                solutions
            )

            query = "search"("a", 2,"X")
            solutions = solver.solve(query).toList()
            assertSolutionEquals(
                ktListOf(
                    query.yes("X" to "a"(1)),
                    query.yes("X" to "a"(1, 2))
                ),
                solutions
            )

            query = "search"("b", 2,"X")
            solutions = solver.solve(query).toList()
            assertSolutionEquals(
                ktListOf(query.no()),
                solutions
            )
        }
    }
}