package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.parallel.cluster.LocalCluster
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

// The statements the catalog is asked to evaluate, whatever they are
class CatalogRequestsTest {
    private fun cluster() = LocalCluster(parts = 2).also { it.load("r1 :=> p(x).\nr2 : p(X) => q(X).") }

    // its answer an error, and the cluster as it was for the next ones
    @Test
    fun aStatementThatCannotBeReadIsAnError() {
        val cluster = cluster()
        assertFailsWith<Arg2pException> { cluster.solve("p(") }
        assertEquals(1, cluster.solve("q(x)").inArgs.size)
    }

    // the variables of the statement are its own, whatever their names
    @Test
    fun theVariablesOfAStatementAreItsOwn() {
        val cluster = cluster()
        listOf("p(A)", "p(L)", "q(S)").forEach { assertEquals(1, cluster.solve(it).inArgs.size, it) }
    }
}
