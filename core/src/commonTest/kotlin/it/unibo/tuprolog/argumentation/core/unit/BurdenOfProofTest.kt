package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.SolveOptions
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

    // The statement labellings of the theory (each as In/Out/Und), within a time limit
    private fun labellings(theory: String): List<String> {
        val solver = solverWithTheory("$theory\n$config")
        val goal = Struct.parse("buildLabelSets(In, Out, Und)", solver.operators)
        return solver
            .solve(goal, SolveOptions.allLazilyWithTimeout(20_000))
            .map { solution ->
                check(!solution.isHalt) { "${solution.exception}" }
                if (solution.isYes) {
                    val bindings = solution.substitution.entries.associate { it.key.name to it.value.toString() }
                    listOf("In", "Out", "Und").joinToString(" ") { bindings.getValue(it) }
                } else {
                    "no"
                }
            }.toList()
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
