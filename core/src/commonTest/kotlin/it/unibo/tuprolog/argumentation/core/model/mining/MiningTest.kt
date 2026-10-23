package it.unibo.tuprolog.argumentation.core.model.mining

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertTrue

/** The arguments mined from a graph, identified in order of their number of sub-arguments first */
class MiningTest {
    private val config =
        """
        graphBuildMode(standard_af).
        statementLabellingMode(statement).
        argumentLabellingMode(grounded).
        orderingPrinciple(last).
        orderingComparator(elitist).
        graphExtension(standardPref).
        """.trimIndent()

    @Test
    fun argumentsWithFewerSubArgumentsComeFirst() {
        // b and c have three rules each, but c rests on one sub-argument (y), b on two (p and q)
        val theory = "f1 :=> p.\nf2 :=> q.\nr1 : p, q => b.\nr2 : [] => x.\nr3 : x => y.\nr4 : y => c.\n$config"
        val solver = solverWithTheory(theory)
        val solution = solver.solve(Struct.parse("buildLabelSets(_, _, _), context_active(B)", solver.operators)).first()
        val active = solution.substitution.entries.first { it.key.name == "B" }
        val branch =
            active.value
                .castToInteger()
                .intValue
                .toInt()
        val order = solver.arguments(branch).map { it.conclusion }
        assertTrue(order.indexOf("c") < order.indexOf("b"), "$order")
    }
}
