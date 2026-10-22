package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

/** The binary statement labelling: each statement labelled once, however many arguments conclude it */
class BinaryStatementLabellingTest {
    private val config =
        """
        graphBuildMode(standard_af).
        statementLabellingMode(statement_binary).
        argumentLabellingMode(grounded).
        orderingPrinciple(last).
        orderingComparator(elitist).
        graphExtension(standardPref).
        """.trimIndent()

    @Test
    fun aStatementOfManyArgumentsIsLabelledOnce() {
        val solver = solverWithTheory("r1 : [] => a.\nr2 : [] => a.\nr3 : [] => b.\nr4 : [] => -b.\n$config")
        val solution = solver.solve(Struct.parse("buildLabelSets(In, Out, Und)", solver.operators)).first()
        val bindings = solution.substitution.entries.associate { it.key.name to it.value.toString() }
        val labels = listOf("In", "Out", "Und").map { bindings.getValue(it) }
        assertEquals(listOf("[[a]]", "[['-'(b)], [b]]", "[]"), labels)
    }
}
