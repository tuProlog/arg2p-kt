package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.TestingUtils.solver
import it.unibo.tuprolog.argumentation.core.TestingUtils.testGoal
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.solve.no
import it.unibo.tuprolog.solve.yes
import it.unibo.tuprolog.theory.Theory
import kotlin.test.Test

class UtilsTest {
    @Test
    fun sort() {
        arg2pScope {
            testGoal("utils" call "sort"(listOf("b", "a", "c", "d"), "X")) {
                listOf(
                    it.yes("X" to listOf("d", "c", "b", "a")),
                    it.no()
                )
            }
        }
    }

    @Test
    fun subtractList() {
        arg2pScope {
            testGoal("utils" call "subtract"(listOf("b", "a", "c", "d", "a"), listOf("b", "a"), "X")) {
                listOf(
                    it.yes("X" to listOf("c", "d"))
                )
            }
        }
    }

    @Test
    fun isEmptyList() {
        arg2pScope {
            testGoal("utils" call "isEmptyList"(emptyLogicList)) {
                listOf(it.yes())
            }

            testGoal("utils" call "isEmptyList"(listOf("a"))) {
                listOf(it.no())
            }
        }
    }

    @Test
    fun appendaList() {
        arg2pScope {
            testGoal("utils" call "appendLists"(listOf(listOf("a", "b"), listOf("c", "d"), listOf("e", "f")), "X")) {
                listOf(
                    it.yes("X" to listOf("a", "b", "c", "d", "e", "f"))
                )
            }

            testGoal("utils" call "appendLists"(listOf(listOf("a", "b")), "X")) {
                listOf(
                    it.yes("X" to listOf("a", "b"))
                )
            }
        }
    }

    @Test
    fun search() {
        arg2pScope {
            val solver = solver(
                Theory.Companion.of(
                    listOf(
                        fact { "a"(1) },
                        fact { "a"(1, 2) },
                        fact { "a"(1, 2, 3) },
                        fact { "a"(1, 2, 3, 4) }
                    )
                )
            )

            testGoal("utils" call "search"("a", 4, "X"), solver) {
                listOf(
                    it.yes("X" to "a"(1)),
                    it.yes("X" to "a"(1, 2)),
                    it.yes("X" to "a"(1, 2, 3)),
                    it.yes("X" to "a"(1, 2, 3, 4))
                )
            }

            testGoal("utils" call "search"("a", 2, "X"), solver) {
                listOf(
                    it.yes("X" to "a"(1)),
                    it.yes("X" to "a"(1, 2))
                )
            }

            testGoal("utils" call "search"("b", 2, "X"), solver) {
                listOf(it.no())
            }
        }
    }
}
