package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.argumentation.federation.Federation
import it.unibo.tuprolog.argumentation.federation.MemoryLinks
import it.unibo.tuprolog.argumentation.parallel.GoalSolver
import it.unibo.tuprolog.argumentation.parallel.ThreadedSolver
import kotlin.test.AfterTest
import kotlin.test.assertEquals

// The scenarios on the threads of the JVM, the rules spread over three partitions
class ThreadedGoalSolverTest : GoalSolverScenarios() {
    private val solvers = mutableListOf<ThreadedSolver>()

    override fun parallel(
        theory: String,
        flags: Flags,
    ): GoalSolver =
        ThreadedSolver(threads = 3, flags = flags)
            .also {
                solvers += it
                it.load(theory)
            }.goals

    override fun configure(flags: Flags) {
        solvers.last().flags = flags
    }

    @AfterTest
    fun close() = solvers.forEach { it.close() }
}

// The scenarios on two federated parallel solvers of the JVM, the graph built by the workers of both
class FederatedGoalSolverTest : GoalSolverScenarios() {
    private val solvers = mutableListOf<ThreadedSolver>()
    private val federations = mutableListOf<Federation>()

    override fun parallel(
        theory: String,
        flags: Flags,
    ): GoalSolver {
        val solving = ThreadedSolver(threads = 1, parts = 2, flags = flags)
        val lending = ThreadedSolver(threads = 2)
        solvers += listOf(solving, lending)
        val federation = Federation(solving, lends = false).also { federations += it }
        MemoryLinks.connect(federation.site, Federation(lending).site)
        solving.load(theory)
        return solving.goals
    }

    // the flags of the parallel solver solving the goals, its peer kept
    override fun configure(flags: Flags) {
        val (solving, lending) = solvers.takeLast(2)
        solving.flags = flags
        assertEquals(setOf(lending.id), federations.last().peers)
    }

    @AfterTest
    fun close() = solvers.forEach { it.close() }
}
