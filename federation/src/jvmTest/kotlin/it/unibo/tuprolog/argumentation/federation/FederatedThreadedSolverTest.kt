package it.unibo.tuprolog.argumentation.federation

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.federation.Federation
import it.unibo.tuprolog.argumentation.federation.MemoryLinks
import it.unibo.tuprolog.argumentation.parallel.EquivalenceScenarios
import it.unibo.tuprolog.argumentation.parallel.ThreadedSolver
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import kotlin.test.AfterTest

// The scenarios on three federated parallel solvers of the JVM, each on threads of its own, their links delivering on the
// threads of the senders: the sites run in parallel, as on three machines
class FederatedThreadedSolverTest : EquivalenceScenarios() {
    private val solvers = mutableListOf<ThreadedSolver>()

    override fun distributed(
        theory: String,
        flags: Flags,
    ): (String) -> EvaluationResponse {
        val (evaluating, first, second) =
            listOf(ThreadedSolver(threads = 1, flags = flags), ThreadedSolver(threads = 2), ThreadedSolver(threads = 2))
        solvers += listOf(evaluating, first, second)
        val (toEvaluate, one, other) = listOf(evaluating, first, second).map { Federation(it).site }
        MemoryLinks.connect(toEvaluate, one)
        MemoryLinks.connect(toEvaluate, other)
        MemoryLinks.connect(one, other)
        evaluating.load(theory)
        return evaluating::solve
    }

    @AfterTest
    fun close() = solvers.forEach { it.close() }
}
