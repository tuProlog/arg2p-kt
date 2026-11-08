package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.ThreadedSolver
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import kotlin.test.AfterTest

// The scenarios on the threads of the JVM, the rules spread over three partitions
class ThreadedSolverTest : EquivalenceScenarios() {
    private val solvers = mutableListOf<ThreadedSolver>()

    override fun distributed(
        theory: String,
        flags: Flags,
    ): (String) -> EvaluationResponse =
        ThreadedSolver(threads = 3, flags = flags).also {
            solvers += it
            it.load(theory)
        }::solve

    @AfterTest
    fun close() = solvers.forEach { it.close() }
}
