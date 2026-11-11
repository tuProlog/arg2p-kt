package it.unibo.tuprolog.argumentation.federation

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.EquivalenceScenarios
import it.unibo.tuprolog.argumentation.parallel.ThreadedSolver
import it.unibo.tuprolog.argumentation.parallel.host.HostCodec
import it.unibo.tuprolog.argumentation.parallel.host.HostFrame
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import kotlin.concurrent.thread
import kotlin.test.AfterTest
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

// The scenarios on three sites linked as a star: the two lending their threads have no link between them, their consumers
// reaching the store of the graph on the evaluating one
class StarThreadedSolverTest : EquivalenceScenarios() {
    private val solvers = mutableListOf<ThreadedSolver>()

    override fun distributed(
        theory: String,
        flags: Flags,
    ): (String) -> EvaluationResponse {
        val evaluating = ThreadedSolver(threads = 1, flags = flags)
        val first = ThreadedSolver(threads = 2)
        val second = ThreadedSolver(threads = 1)
        solvers += listOf(evaluating, first, second)
        val hub = Federation(evaluating).site
        MemoryLinks.connect(hub, Federation(first).site)
        MemoryLinks.connect(hub, Federation(second).site)
        evaluating.load(theory)
        return evaluating::solve
    }

    @AfterTest
    fun close() = solvers.forEach { it.close() }
}

// A site running consumers of a graph leaving it, while it is built or once it is: the graph is built again with the
// sites left, rather than waited for
class SiteLeavingTest {
    // a chain whose last conclusion is rebutted by an argument resting on another chain, and a third chain
    private val theory =
        (1..6).joinToString("\n") { "r$it : a${it - 1} => a$it." } +
            "\nf0 :=> a0.\n" +
            (1..6).joinToString("\n") { "s$it : b${it - 1} => b$it." } +
            "\ng0 :=> b0.\nt : b6 => -a6.\nsup(t, r6).\nu0 :=> c0.\nu1 : c0 => c1.\n"

    private val solvers = mutableListOf<ThreadedSolver>()

    private fun parallel(threads: Int) = ThreadedSolver(threads = threads, parts = 3).also { solvers += it }

    @AfterTest
    fun close() = solvers.forEach { it.close() }

    @Test
    fun whileItIsBuilt() {
        val evaluating = parallel(1)
        val site = Federation(evaluating).site
        val leaving = Federation(parallel(2)).site
        val staying = Federation(parallel(2)).site
        // the leaving site drops its link after a few answers of the store to its consumers
        MemoryLinks.connect(site, Leaving(leaving, after = 3))
        MemoryLinks.connect(site, staying)
        evaluating.load(theory)
        assertEquals(listOf("a6"), evaluating.solve("a6").outArgs.map { it.claim })
        assertEquals(setOf(staying.id), site.peerSites)
        assertEquals(listOf("b6"), evaluating.solve("b6").inArgs.map { it.claim })
    }

    @Test
    fun onceItIsBuilt() {
        val evaluating = parallel(1)
        val site = Federation(evaluating).site
        val leaving = Closable(Federation(parallel(2)).site)
        val staying = Federation(parallel(2)).site
        MemoryLinks.connect(site, leaving)
        MemoryLinks.connect(site, staying)
        evaluating.load(theory)
        assertEquals(listOf("a6"), evaluating.solve("a6").outArgs.map { it.claim })
        leaving.close()
        assertEquals(setOf(staying.id), site.peerSites)
        assertEquals(listOf("c1"), evaluating.solve("c1").inArgs.map { it.claim })
    }

    // A site whose links can be closed
    private class Closable(
        private val site: Site,
    ) : LinkListener by site {
        private val links = mutableListOf<Link>()

        override fun opened(link: Link) {
            synchronized(links) { links += link }
            site.opened(link)
        }

        fun close() = synchronized(links) { links.toList() }.forEach { it.close() }
    }

    // A site whose link closes after it received [after] answers for its consumers
    private class Leaving(
        private val site: Site,
        private var after: Int,
    ) : LinkListener by site {
        override fun received(
            link: Link,
            frame: String,
        ) {
            val closing = synchronized(this) { isAnswer(frame) && --after == 0 }
            if (closing) return link.close()
            site.received(link, frame)
        }

        private fun isAnswer(frame: String) =
            (FrameCodec.decode(frame) as? Frame.Host)?.let { HostCodec.decode(it.frame) is HostFrame.Answer } == true
    }
}

// The scenarios on two parallel solvers laid out as those of JavaScript, federated, the one lending running its consumers on its threads
class DetachedFederatedSolverTest : EquivalenceScenarios() {
    private val solvers = mutableListOf<ThreadedSolver>()

    override fun distributed(
        theory: String,
        flags: Flags,
    ): (String) -> EvaluationResponse {
        val evaluating = ThreadedSolver.detached(threads = 2, flags = flags)
        val lending = ThreadedSolver.detached(threads = 3)
        solvers += listOf(evaluating, lending)
        MemoryLinks.connect(Federation(evaluating).site, Federation(lending).site)
        evaluating.load(theory)
        return evaluating::solve
    }

    @AfterTest
    fun close() = solvers.forEach { it.close() }
}

// Two such parallel solvers lending to each other, both solving at once: the consumer of one finds the threads of the other busy
// with its own, and neither can wait for the other (each graph is to end by itself)
class DetachedMutualSolverTest {
    private val theory =
        (1..6).joinToString("\n") { "r$it : a${it - 1} => a$it." } + "\nf0 :=> a0.\n" +
            (1..6).joinToString("\n") { "s$it : b${it - 1} => b$it." } +
            "\ng0 :=> b0.\nt : b6 => -a6.\nsup(t, r6).\nu0 :=> c0.\nu1 : c0 => c1.\n"

    @Test
    fun solversLendingToEachOtherBothFinish() {
        val solvers = List(2) { ThreadedSolver.detached(threads = 2, parts = 3) }
        try {
            MemoryLinks.connect(Federation(solvers[0]).site, Federation(solvers[1]).site)
            solvers.forEach { it.load(theory) }
            val failures = java.util.concurrent.CopyOnWriteArrayList<Throwable>()
            val solving =
                solvers.map { parallel ->
                    thread(isDaemon = true) {
                        try {
                            repeat(4) {
                                listOf("a6", "b6", "c1").forEach { statement -> parallel.solve(statement) }
                            }
                        } catch (failure: Throwable) {
                            failures += failure
                        }
                    }
                }
            solving.forEach { it.join(90_000) }
            assertTrue(solving.none { it.isAlive }, "still waiting: the two graphs wait for each other")
            assertEquals(emptyList(), failures.map { it.message })
        } finally {
            solvers.forEach { it.close() }
        }
    }
}
