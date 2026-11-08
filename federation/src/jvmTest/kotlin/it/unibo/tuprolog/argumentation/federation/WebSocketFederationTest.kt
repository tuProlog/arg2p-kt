package it.unibo.tuprolog.argumentation.federation

import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.EquivalenceScenarios
import it.unibo.tuprolog.argumentation.parallel.ThreadedSolver
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import kotlin.test.AfterTest
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

// The scenarios on three parallel solvers federated over WebSockets on this machine: the two lending their threads connect
// to the evaluating one
class WebSocketFederationTest : EquivalenceScenarios() {
    private val solvers = mutableListOf<ThreadedSolver>()

    override fun distributed(
        theory: String,
        flags: Flags,
    ): (String) -> EvaluationResponse {
        val evaluating = ThreadedSolver(threads = 1, flags = flags).also { solvers += it }
        val federation = Federation(evaluating)
        val port = federation.listen(0, "127.0.0.1")
        repeat(2) { federated(2).connect("ws://127.0.0.1:$port") }
        awaitPeers(federation, 2)
        evaluating.load(theory)
        return evaluating::solve
    }

    @Test
    fun aSolverLeavingIsNoLongerAPeer() {
        val evaluating = federated(1)
        val port = evaluating.listen(0, "127.0.0.1")
        ThreadedSolver(threads = 1).use { leaving ->
            Federation(leaving).connect("ws://127.0.0.1:$port")
            awaitPeers(evaluating, 1)
        }
        awaitPeers(evaluating, 0)
        assertEquals(emptySet(), evaluating.peers)
    }

    @Test
    fun connectingWhereNoSiteListensFails() {
        assertFailsWith<Arg2pException> { federated(1).connect("ws://127.0.0.1:1") }
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
