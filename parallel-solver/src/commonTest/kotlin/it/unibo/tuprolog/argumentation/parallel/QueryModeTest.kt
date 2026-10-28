package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.argumentation.parallel.cluster.LocalCluster
import kotlin.test.Test
import kotlin.test.assertEquals

// Query mode: the graph built on the rules the evaluations depend on, as the semantics declares
class QueryModeTest {
    // two parts no attack relates: a and b, and c, d and -d
    private val theory = "r1 :=> a.\nr2 : a => b.\nr3 :=> c.\nr4 : c => d.\nr5 :=> -d.\n"

    private fun cluster(settings: FlagsBuilder) = LocalCluster(parts = 2, settings = settings).also { it.load(theory) }

    @Test
    fun theGraphIsBuiltOnTheRulesTheQueryDependsOn() {
        val cluster = cluster(FlagsBuilder(graphExtensions = emptyList()))
        assertEquals(1, cluster.solve("b").inArgs.size)
        assertEquals(2, cluster.site.argumentsBuilt)
        // a later query needing more: the graph grows
        assertEquals(1, cluster.solve("d").undArgs.size)
        assertEquals(5, cluster.site.argumentsBuilt)
        // and one it covers leaves it as it is
        assertEquals(1, cluster.solve("a").inArgs.size)
        assertEquals(5, cluster.site.argumentsBuilt)
    }

    @Test
    fun theAttackersOfTheArgumentsAreBuiltAsWell() {
        val cluster = cluster(FlagsBuilder(graphExtensions = emptyList()))
        assertEquals(1, cluster.solve("d").undArgs.size)
        assertEquals(3, cluster.site.argumentsBuilt)
    }

    @Test
    fun theWholeGraphIsBuiltWithNoQueryMode() {
        val cluster = cluster(FlagsBuilder(graphExtensions = emptyList(), queryMode = false))
        assertEquals(1, cluster.solve("b").inArgs.size)
        assertEquals(5, cluster.site.argumentsBuilt)
    }

    @Test
    fun theWholeGraphIsBuiltForASemanticsDependingOnIt() {
        val cluster = cluster(FlagsBuilder(graphExtensions = emptyList(), argumentLabellingMode = "stable"))
        cluster.solve("b")
        assertEquals(5, cluster.site.argumentsBuilt)
    }

    @Test
    fun aConnectedSemanticsBuildsWhatTheQueryIsConnectedTo() {
        // naive: the arguments for a and for b, which a supports
        val cluster = cluster(FlagsBuilder(graphExtensions = emptyList(), argumentLabellingMode = "naive"))
        cluster.solve("a")
        assertEquals(2, cluster.site.argumentsBuilt)
    }

    @Test
    fun aChangeOfTheTheoryBuildsTheGraphAnew() {
        val cluster = cluster(FlagsBuilder(graphExtensions = emptyList()))
        assertEquals(1, cluster.solve("b").inArgs.size)
        cluster.add("r6 :=> -a")
        assertEquals(1, cluster.solve("b").undArgs.size)
        assertEquals(3, cluster.site.argumentsBuilt)
    }
}
