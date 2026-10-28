package it.unibo.tuprolog.argumentation.behaviour

import it.unibo.tuprolog.argumentation.behaviour.Behaviour.solver
import kotlin.test.Test
import kotlin.test.assertEquals

/** The burden of proof extension (graphExtension(bp)) on attack cycles, several burdens, and none */
class BurdenOfProofTest {
    private val config =
        """
        statementLabellingMode(statement).
        argumentLabellingMode(grounded).
        orderingPrinciple(last).
        orderingComparator(elitist).
        graphExtension(standardPref).
        graphExtension(bp).
        """.trimIndent()

    // The statement labellings of the theory (each as In/Out/Und)
    private fun labellings(theory: String): List<String> =
        solver("$theory\n$config")
            .solve("buildLabelSets(In, Out, Und)")
            .toList()
            .map { solution ->
                check(!solution.isHalt) { "${solution.error}" }
                if (solution.isYes) listOf("In", "Out", "Und").joinToString(" ") { solution.text(it)!! } else "no"
            }

    @Test
    fun burdensOnArgumentsInAttackCycles() {
        val theory = "r1 : [] => a.\nr2 : [] => -a.\nr3 : [] => b.\nr4 : [] => -b.\nb1 : [] => bp(a).\nb2 : [] => bp(b)."
        assertEquals(1, labellings(theory).size)
    }

    @Test
    fun twoBurdensOnTheSameStatement() {
        val theory = "r1 : [] => a.\nr2 : [] => -a.\nr3 : [] => c.\nb1 : [] => bp(a).\nb2 : c => bp(a)."
        assertEquals(1, labellings(theory).filter { it != "no" }.size)
    }

    @Test
    fun noBurdens() {
        assertEquals(1, labellings("r1 : [] => a.").size)
    }
}
