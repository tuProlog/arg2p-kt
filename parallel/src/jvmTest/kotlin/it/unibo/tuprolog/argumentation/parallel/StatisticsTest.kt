package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Flags
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull
import kotlin.test.assertTrue

// What a parallel solver tells of the evaluations it answered
class StatisticsTest {
    // two parts no attack relates: a and b, and c, d and -d
    private val theory = "r1 :=> a.\nr2 : a => b.\nr3 :=> c.\nr4 : c => d.\nr5 :=> -d.\n"

    @Test
    fun theArgumentsBuiltForEachEvaluation() {
        val parallel = ThreadedSolver(threads = 2, parts = 2, flags = Flags.defaults().graphExtensions())
        assertNull(parallel.evaluation)
        parallel.load(theory)
        parallel.solve("b")
        val first = parallel.evaluation!!
        assertEquals("directional", first.locality)
        assertEquals(2, first.workers.size)
        assertEquals(listOf(parallel.id), first.workers.map { it.site }.distinct())
        assertEquals(2, first.added)
        assertEquals(first.added, first.workers.sumOf { it.arguments })
        assertEquals(2, first.arguments)
        // query mode: a later evaluation builds its graph anew, the same one again builds nothing
        parallel.solve("d")
        assertEquals(3, parallel.evaluation!!.added)
        assertEquals(3, parallel.evaluation!!.arguments)
        assertEquals(2, parallel.evaluation!!.attacks)
        parallel.solve("d")
        assertEquals(0, parallel.evaluation!!.added)
        // the whole graph, for a goal asking for it
        parallel.goals.solve("solve(In, Out, Und)").next()
        assertEquals("wholeGraph", parallel.evaluation!!.locality)
        assertEquals(5, parallel.evaluation!!.added)
        // a goal using no graph leaves them as they are
        val evaluation = parallel.evaluation
        parallel.goals.solve("member(X, [1, 2])").next()
        assertTrue(evaluation === parallel.evaluation)
    }

    // a graph the client solver builds alone: an abstract framework, with no other consumer
    @Test
    fun aGraphBuiltAlone() {
        val parallel = ThreadedSolver(threads = 2, parts = 2)
        parallel.load(theory)
        parallel.solve("b")
        parallel.load("graphBuildMode(abstract).\na <-> b.\nb -> c.\n")
        parallel.goals.solve("solve(In, Out, Und)").next()
        val evaluation = parallel.evaluation!!
        assertEquals("alone", evaluation.locality)
        assertEquals(3, evaluation.arguments)
        assertEquals(3, evaluation.attacks)
        assertEquals(0, evaluation.added)
        assertTrue(evaluation.workers.isEmpty())
    }
}
