package it.unibo.tuprolog.argumentation.federation

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.EquivalenceScenarios
import it.unibo.tuprolog.argumentation.parallel.ThreadedSolver
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import kotlin.test.AfterTest
import kotlin.test.Test
import kotlin.test.assertEquals

// The scenarios on three parallel solvers federated over a medium of the application (ExternalLink), as in JavaScript: the
// two lending their threads linked to the evaluating one, the frames delivered on the threads of the senders
class ExternalLinkFederationTest : EquivalenceScenarios() {
    private val solvers = mutableListOf<ThreadedSolver>()

    override fun distributed(
        theory: String,
        flags: Flags,
    ): (String) -> EvaluationResponse {
        val evaluating = ThreadedSolver(threads = 1, flags = flags).also { solvers += it }
        val federation = Federation(evaluating)
        repeat(2) { medium(federated(2), federation) }
        awaitPeers(federation, 2)
        evaluating.load(theory)
        return evaluating::solve
    }

    @Test
    fun closingTheFederationClosesItsLinks() {
        val one = federated(1)
        val other = federated(1)
        medium(one, other)
        awaitPeers(one, 1)
        awaitPeers(other, 1)
        one.close()
        awaitPeers(other, 0)
        assertEquals(emptySet(), one.peers)
    }

    // A medium between [one] and [other]: its two ends, both told when it closes
    private fun medium(
        one: Federation,
        other: Federation,
    ) {
        lateinit var first: ExternalLink
        lateinit var second: ExternalLink
        var open = true
        val closing = {
            if (open) {
                open = false
                one.linkClosed(first)
                other.linkClosed(second)
            }
        }
        first = ExternalLink({ frame -> if (open) other.linkReceived(second, frame) }, closing, outgoing = true)
        second = ExternalLink({ frame -> if (open) one.linkReceived(first, frame) }, closing)
        one.linkOpened(first)
        other.linkOpened(second)
    }

    private fun federated(threads: Int) = Federation(ThreadedSolver(threads = threads).also { solvers += it })

    // The links greet each other a moment after they open
    private fun awaitPeers(
        federation: Federation,
        count: Int,
    ) {
        val deadline = System.currentTimeMillis() + 10_000
        while (federation.peers.size != count) {
            check(System.currentTimeMillis() < deadline) { "${federation.peers.size} peers rather than $count" }
            Thread.sleep(5)
        }
    }

    @AfterTest
    fun close() {
        solvers.forEach { it.close() }
        solvers.clear()
    }
}
