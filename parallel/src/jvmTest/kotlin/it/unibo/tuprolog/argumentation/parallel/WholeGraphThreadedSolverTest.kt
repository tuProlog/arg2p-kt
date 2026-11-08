package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.ThreadedSolver
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import kotlin.test.AfterTest

// The scenarios on the whole graph, on the threads of the JVM: the graph built by the solvers of the partitions together,
// the solver asked one of them (see SharedGraph)
class WholeGraphThreadedSolverTest : EquivalenceScenarios() {
    private val solvers = mutableListOf<ThreadedSolver>()

    override val wholeGraph = true

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
