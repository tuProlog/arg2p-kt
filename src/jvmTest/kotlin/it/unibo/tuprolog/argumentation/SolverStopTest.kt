package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.argumentation.parallel.ThreadedSolver
import java.util.concurrent.CompletableFuture
import java.util.concurrent.TimeUnit
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/** A goal being solved by a parallel solver is stopped at once, and the parallel solver solves the next ones as before */
class SolverStopTest {
    // a Prolog check that takes long, in a rule (checked by a member) and as a goal (solved by the client solver)
    private val slow = "slow :- between(1, 100000000, _), fail.\nslow.\nr1 : prolog(slow) => a.\nr2 :=> b.\n"

    private val conflict = "r1 : [] => a.\nr2 : [] => -a.\nsup(r1, r2).\n"

    // [goal] stopped once it is being solved: its solution is a halt, at once
    private fun stopped(
        parallel: ThreadedSolver,
        goal: String,
    ) {
        val solution = CompletableFuture.supplyAsync { parallel.goals.solve(goal).next() }
        Thread.sleep(500)
        parallel.goals.stop()
        val halt = solution.get(5, TimeUnit.SECONDS)
        assertTrue(halt.isHalt, goal)
        assertEquals("The evaluation was stopped", halt.error)
    }

    // the next goal, on another theory, answered as by the standard solver
    private fun answersAfter(parallel: ThreadedSolver) {
        parallel.load(conflict)
        val expected = Arg2p.of(conflict).solve("solve(In, Out, Und)").next()
        val solution = parallel.goals.solve("solve(In, Out, Und)").next()
        assertEquals(expected.toString(), solution.toString())
        assertEquals(Arg2p.of(conflict).answer("-a"), parallel.goals.answer("-a"))
    }

    @Test
    fun aGraphBeingBuilt() =
        ThreadedSolver(threads = 2).use { parallel ->
            parallel.load(slow)
            stopped(parallel, "solve(In, Out, Und)")
            answersAfter(parallel)
        }

    @Test
    fun aGoalOfTheSolver() =
        ThreadedSolver(threads = 2).use { parallel ->
            parallel.load(slow)
            stopped(parallel, "slow")
            answersAfter(parallel)
        }

    @Test
    fun nothingToStop() =
        ThreadedSolver(threads = 2).use { parallel ->
            parallel.load(conflict)
            parallel.goals.stop()
            answersAfter(parallel)
        }
}
