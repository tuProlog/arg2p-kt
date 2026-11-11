package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.solverWithTheory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

/** catch/3 catches the balls of its goal only while the goal runs: not those raised after the goal exited */
class CatchTest {
    private val theory =
        "q(R) :- var(R), throw(boom).\nq(_).\n" +
            "loop(0) :- !.\nloop(N) :- catch(true, _, true), N1 is N - 1, loop(N1)."

    // Each solution of [goal]: the value of X, or no, or the error that halted it
    private fun solutions(goal: String): List<String> {
        val solver = solverWithTheory(theory)
        return solver
            .solve(Struct.parse(goal, solver.operators))
            .map { solution ->
                when {
                    solution.isYes ->
                        solution.substitution.entries
                            .firstOrNull { it.key.name == "X" }
                            ?.value
                            ?.toString() ?: "yes"
                    solution.isHalt -> "error: " + solution.exception!!.message
                    else -> "no"
                }
            }.toList()
    }

    @Test
    fun anErrorAfterTheGoalExitedIsNotCaught() {
        val after = solutions("catch(true, _, fail), X is foo + 1")
        assertEquals(1, after.size)
        assertEquals(true, after.single().startsWith("error"), after.toString())
        assertEquals(true, solutions("catch(true, E, X = caught(E)), q(X)").single().startsWith("error"))
    }

    @Test
    fun aGoalThatExitsDeterministicallyLeavesNoChoicePoint() {
        assertEquals(listOf("yes"), solutions("catch(true, _, true)"))
        assertEquals(listOf("yes"), solutions("loop(1000)"))
    }

    @Test
    fun theGoalsBallsAreStillCaught() {
        assertEquals(listOf("caught(boom)"), solutions("catch(q(_), E, X = caught(E))"))
        assertEquals(listOf("[1, 2, 3]"), solutions("findall(Y, catch(member(Y, [1, 2, 3]), _, true), X)"))
    }

    @Test
    fun backtrackingIntoTheGoalMakesTheCatchActiveAgain() {
        // the goal exits with X0 = 1 and a choice point left; failing after it enters the goal again, which throws
        assertEquals(
            listOf("[1, caught]"),
            solutions("findall(Y, catch((member(Z, [1, 2]), (Z =:= 2 -> throw(two) ; Y = Z)), two, Y = caught), X)"),
        )
        // (then no: X = 3 is tried and fails)
        assertEquals(listOf("2", "no"), solutions("catch(member(X, [1, 2, 3]), _, true), X > 1, X < 3"))
    }

    @Test
    fun anErrorAfterANondeterministicExitIsNotCaught() {
        assertEquals(true, solutions("catch(member(X, [1, 2]), _, true), throw(after)").first().startsWith("error"))
    }
}
