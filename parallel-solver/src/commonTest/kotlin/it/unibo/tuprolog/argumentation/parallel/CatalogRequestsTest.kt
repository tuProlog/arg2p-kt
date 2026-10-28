package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.parallel.cluster.LocalCluster
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertTrue

// What the catalog is asked to evaluate, whatever it is: an error is an answer, and the cluster goes on
class CatalogRequestsTest {
    private val theory = "r1 :=> p(x).\nr2 : p(X) => q(X)."

    private fun cluster() = LocalCluster(parts = 2).also { it.load(theory) }

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

    @Test
    fun aTheoryThatCannotBeReadIsAnError() {
        val cluster = LocalCluster(parts = 2).also { it.load("r1 : [] => .") }
        assertFailsWith<Arg2pException> { cluster.solve("a") }
        cluster.load(theory)
        assertEquals(1, cluster.solve("q(x)").inArgs.size)
    }

    // as the standard solver: ABA with a defeasible rule, ABA+ with no weak contraposition
    @Test
    fun aTheoryItsProfileRefusesIsAnError() {
        val defeasible = LocalCluster(parts = 2).also { it.load("graphBuildMode(aba).\nassumption(a).\nr : a => b.") }
        assertTrue("defeasible" in assertFailsWith<Arg2pException> { defeasible.solve("b") }.message.orEmpty())
        val noContraposition =
            LocalCluster(parts = 2).also {
                it.load(
                    "graphBuildMode(aba_plus).\nargumentLabellingMode(preferred).\nassumption(a0).\nassumption(a1).\n" +
                        "contrary(a0, x).\ncontrary(a1, y).\nr0 : a0, a1 -> x.\nsup(a0, a1).",
                )
            }
        assertTrue("weak contraposition" in assertFailsWith<Arg2pException> { noContraposition.solve("x") }.message.orEmpty())
    }
}
