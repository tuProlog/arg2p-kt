package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import kotlin.test.AfterTest

// The scenarios on a parallel solver laid out as one of JavaScript: its solver and its consumers on threads of their own, which
// wait, apart from its node, the two talking through frames as text alone
class DetachedSolverTest : EquivalenceScenarios() {
    private val solvers = mutableListOf<ThreadedSolver>()

    override fun distributed(
        theory: String,
        flags: Flags,
    ): (String) -> EvaluationResponse {
        val parallel = ThreadedSolver.detached(threads = 3, flags = flags).also { solvers += it }
        parallel.load(theory)
        return parallel::solve
    }

    @AfterTest
    fun close() = solvers.forEach { it.close() }
}
