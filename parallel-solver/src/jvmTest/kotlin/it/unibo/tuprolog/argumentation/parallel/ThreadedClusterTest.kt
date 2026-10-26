package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.argumentation.parallel.cluster.ThreadedCluster
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import kotlin.test.AfterTest

// The scenarios on the threads of the JVM, the rules spread over three partitions
class ThreadedClusterTest : EquivalenceScenarios() {
    private val clusters = mutableListOf<ThreadedCluster>()

    override fun distributed(
        theory: String,
        settings: FlagsBuilder,
    ): (String) -> EvaluationResponse =
        ThreadedCluster(threads = 3, settings = settings).also {
            clusters += it
            it.load(theory)
        }::solve

    @AfterTest
    fun close() = clusters.forEach { it.close() }
}
