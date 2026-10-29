package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.argumentation.parallel.cluster.ThreadedCluster
import java.util.concurrent.CompletableFuture
import java.util.concurrent.TimeUnit
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/** A goal being solved by a cluster is stopped at once, and the cluster solves the next ones as before */
class ClusterStopTest {
    // a Prolog check that takes long, in a rule (checked by a worker) and as a goal (solved by the catalog)
    private val slow = "slow :- between(1, 100000000, _), fail.\nslow.\nr1 : prolog(slow) => a.\nr2 :=> b.\n"

    private val conflict = "r1 : [] => a.\nr2 : [] => -a.\nsup(r1, r2).\n"

    // [goal] stopped once it is being solved: its solution is a halt, at once
    private fun stopped(
        cluster: ThreadedCluster,
        goal: String,
    ) {
        val solution = CompletableFuture.supplyAsync { cluster.solver.solve(goal).next() }
        Thread.sleep(500)
        cluster.solver.stop()
        val halt = solution.get(5, TimeUnit.SECONDS)
        assertTrue(halt.isHalt, goal)
        assertEquals("The evaluation was stopped", halt.error)
    }

    // the next goal, on another theory, answered as by the standard solver
    private fun answersAfter(cluster: ThreadedCluster) {
        cluster.load(conflict)
        val expected = Arg2p.of(conflict).solve("buildLabelSets(In, Out, Und)").next()
        val solution = cluster.solver.solve("buildLabelSets(In, Out, Und)").next()
        assertEquals(expected.toString(), solution.toString())
        assertEquals(Arg2p.of(conflict).answer("-a"), cluster.solver.answer("-a"))
    }

    @Test
    fun aGraphBeingBuilt() =
        ThreadedCluster(threads = 2).use { cluster ->
            cluster.load(slow)
            stopped(cluster, "buildLabelSets(In, Out, Und)")
            answersAfter(cluster)
        }

    @Test
    fun aGoalOfTheCatalog() =
        ThreadedCluster(threads = 2).use { cluster ->
            cluster.load(slow)
            stopped(cluster, "slow")
            answersAfter(cluster)
        }

    @Test
    fun nothingToStop() =
        ThreadedCluster(threads = 2).use { cluster ->
            cluster.load(conflict)
            cluster.solver.stop()
            answersAfter(cluster)
        }
}
