package it.unibo.tuprolog.argumentation.core.modules.utils

import it.unibo.tuprolog.argumentation.core.TestingUtils.testGoal
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.solve.yes
import kotlin.test.Test

class UtilsModuleTest {
    // sort/2 is deterministic: no choice point is left after the solution
    @Test
    fun sort() {
        arg2pScope {
            testGoal("utils" call "sort"(listOf("b", "a", "c", "d"), "X")) {
                listOf(
                    it.yes("X" to listOf("d", "c", "b", "a")),
                )
            }
        }
    }

    @Test
    fun subtractList() {
        arg2pScope {
            testGoal("utils" call "subtract"(listOf("b", "a", "c", "d", "a"), listOf("b", "a"), "X")) {
                listOf(
                    it.yes("X" to listOf("c", "d")),
                )
            }
        }
    }

    @Test
    fun appendaList() {
        arg2pScope {
            testGoal("utils" call "appendLists"(listOf(listOf("a", "b"), listOf("c", "d"), listOf("e", "f")), "X")) {
                listOf(
                    it.yes("X" to listOf("a", "b", "c", "d", "e", "f")),
                )
            }

            testGoal("utils" call "appendLists"(listOf(listOf("a", "b")), "X")) {
                listOf(
                    it.yes("X" to listOf("a", "b")),
                )
            }
        }
    }

    @Test
    fun errorsRaisedInsideAModuleCallReachTheCaller() {
        arg2pScope {
            testGoal(
                "catch"(
                    "utils" call "containsAny"("notAList", listOf("a")),
                    "error"("type_error"("list", "Culprit"), "_"),
                    "true",
                ),
            ) {
                listOf(it.yes("Culprit" to "notAList"))
            }
        }
    }
}
