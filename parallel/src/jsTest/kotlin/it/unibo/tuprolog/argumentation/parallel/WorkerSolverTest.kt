package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Flags
import kotlin.js.Promise
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

// A parallel solver of Node.js: its node, its solver and its consumers each on a thread of its own, the solver and the consumers
// waiting on what the node writes them (see SharedInbox)
class WorkerSolverTest {
    // a chain whose last conclusion is rebutted by an argument resting on another chain, and a chain apart
    private val theory =
        (1..6).joinToString("\n") { "r$it : a${it - 1} => a$it." } +
            "\nf0 :=> a0.\n" +
            (1..6).joinToString("\n") { "s$it : b${it - 1} => b$it." } +
            "\ng0 :=> b0.\nt : b6 => -a6.\nsup(t, r6).\nu0 :=> c0.\nu1 : c0 => c1.\n"

    private fun claims(
        parallel: WorkerSolver,
        statement: String,
    ): Promise<List<List<String>>> =
        parallel.solve(statement).then { response ->
            listOf(response.inArgs, response.outArgs, response.undArgs).map { it.map { r -> r.claim } }
        }

    // (the promise a step gives, the next step on its value: as JavaScript flattens promises)
    private fun <T, R> Promise<T>.step(next: (T) -> Promise<R>): Promise<R> = then { next(it) }.unsafeCast<Promise<R>>()

    private fun both(flags: Flags): Promise<Unit> {
        // (the fewest threads for three parts: the node, its solver, two consumers)
        val parallel = WorkerSolver(threads = 3, parts = 3, flags = flags)
        parallel.load(theory)
        return claims(parallel, "a6")
            .step {
                assertEquals(listOf(emptyList(), listOf("a6"), emptyList()), it)
                claims(parallel, "-a6")
            }.step {
                assertEquals(listOf(listOf("-a6"), emptyList(), emptyList()), it)
                claims(parallel, "c1")
            }.step {
                assertEquals(listOf(listOf("c1"), emptyList(), emptyList()), it)
                parallel.goals.solve("solve(In, Out, Und)")
            }.step { it.nextSolution() }
            .step { solution ->
                assertEquals(true, solution.isYes)
                assertEquals(
                    3,
                    parallel.evaluation!!
                        .workers.size,
                )
                parallel.close()
            }
    }

    // (resolved after [millis], on the timers of the thread of the test)
    private fun after(millis: Int): Promise<Unit> = Promise { resolve, _ -> js("setTimeout")({ resolve(Unit) }, millis) }

    // A stop reaches the worker of the solver while it resolves (it reads no message meanwhile): the goal under way ends
    // at once, and the next is answered without waiting for it. slow needs a premise that runs for over ten seconds (on
    // integers that fit a word: beyond, each value is kept by the program).
    @Test
    fun aStopEndsTheGoalUnderWayAndTheNextIsAnswered(): Promise<Unit> {
        // (one part: the worker of the solver builds the graph itself)
        val parallel = WorkerSolver(threads = 2, parts = 1)
        parallel.load(
            "spin(N) :- (N =< 0 -> true ; M is N - 1, spin(M)).\n" +
                "f0 :=> go.\nr0 : go, prolog(spin(250000000)) => slow.\nu0 :=> c0.\nu1 : c0 => c1.\n",
        )
        val now = { js("Date.now()") as Double }
        val slow = parallel.solve("slow").then({ "answered" }, { "stopped" })
        var stoppedAt = 0.0
        return after(300)
            .step {
                parallel.goals.stop()
                stoppedAt = now()
                slow
            }.step {
                assertEquals("stopped", it)
                claims(parallel, "c1")
            }.step {
                assertEquals(listOf(listOf("c1"), emptyList(), emptyList()), it)
                // (well before the loop would end)
                assertTrue(now() - stoppedAt < 3000, "c1 answered ${now() - stoppedAt} ms after the stop")
                parallel.close()
            }
    }

    @Test
    fun theGraphOfAQueryIsBuiltByTheThreads(): Promise<Unit> = both(Flags.defaults())

    @Test
    fun theWholeGraphIsBuiltByTheThreads(): Promise<Unit> = both(Flags.defaults().queryMode(false))
}
