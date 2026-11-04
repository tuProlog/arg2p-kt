package it.unibo.tuprolog.argumentation.core.modules.utils

import it.unibo.tuprolog.argumentation.core.TestingUtils.testGoal
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.solve.no
import it.unibo.tuprolog.solve.yes
import kotlin.test.Test

class UtilsModuleTest {
    // sortDescending/2 is deterministic: no choice point is left after the solution
    @Test
    fun sortDescending() {
        arg2pScope {
            testGoal("utils" call "sortDescending"(listOf("b", "a", "c", "d"), "X")) {
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
    fun subtractManyElements() {
        val removed = (1..20).map { "x$it" }
        arg2pScope {
            testGoal("utils" call "subtract"(listOf("a") + removed + listOf("b", "x3", "c"), removed, "X")) {
                listOf(
                    it.yes("X" to listOf("a", "b", "c")),
                )
            }
        }
    }

    // A variable is removed by itself only, however many the elements removed
    @Test
    fun subtractComparesAsIdentical() {
        val many = (1..20).map { "x$it" }
        arg2pScope {
            testGoal("utils" call "subtract"(listOf("f"("A"), "f"("B")), listOf("f"("A")), listOf("X"))) {
                listOf(
                    it.yes("X" to "f"("B")),
                )
            }
            testGoal("utils" call "subtract"(listOf("f"("A"), "f"("B")), many + "f"("A"), listOf("X"))) {
                listOf(
                    it.yes("X" to "f"("B")),
                )
            }
        }
    }

    @Test
    fun containsNeitherBindsNorFindsWhatIsNotThere() {
        arg2pScope {
            testGoal("utils" call "contains"("f"("X"), listOf("a", "f"("b")))) {
                listOf(it.yes())
            }
            testGoal("utils" call "contains"("c", listOf("a", "b"))) {
                listOf(it.no())
            }
        }
    }

    @Test
    fun containsOnWhatIsNotAListRaises() {
        arg2pScope {
            testGoal(
                "catch"(
                    "utils" call "contains"("a", "notAList"),
                    "error"("type_error"("list", "Culprit"), "_"),
                    "true",
                ),
            ) {
                listOf(it.yes("Culprit" to "notAList"))
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
                    "utils" call "uniqueByFirst"("notAList", "X"),
                    "error"("type_error"("list", "Culprit"), "_"),
                    "true",
                ),
            ) {
                listOf(it.yes("Culprit" to "notAList"))
            }
        }
    }
}
