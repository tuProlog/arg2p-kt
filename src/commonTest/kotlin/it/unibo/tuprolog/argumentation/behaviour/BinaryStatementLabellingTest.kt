package it.unibo.tuprolog.argumentation.behaviour

import it.unibo.tuprolog.argumentation.behaviour.Behaviour.solver
import kotlin.test.Test
import kotlin.test.assertEquals

/** The binary statement labelling: each statement labelled once, however many arguments conclude it */
class BinaryStatementLabellingTest {
    private val config =
        """
        statementLabellingMode(statement_binary).
        argumentLabellingMode(grounded).
        orderingPrinciple(last).
        orderingComparator(elitist).
        graphExtension(standardPref).
        """.trimIndent()

    @Test
    fun aStatementOfManyArgumentsIsLabelledOnce() {
        val solution =
            solver("r1 : [] => a.\nr2 : [] => a.\nr3 : [] => b.\nr4 : [] => -b.\n$config")
                .solve("buildLabelSets(In, Out, Und)")
                .next()
        val labels = listOf("In", "Out", "Und").map { solution.text(it) }
        assertEquals(listOf("[[a]]", "[[-b], [b]]", "[]"), labels)
    }

    // a has an argument IN (r1) and one UND (r2, on c, rebutted by -c): it is IN only
    @Test
    fun aStatementWithAnArgumentInIsOnlyIn() {
        val solution =
            solver("r1 : [] => a.\nr2 : c => a.\nr3 : [] => c.\nr4 : [] => -c.\n$config")
                .solve("buildLabelSets(In, Out, Und)")
                .next()
        val labels = listOf("In", "Out", "Und").map { solution.text(it) }
        assertEquals(listOf("[[a]]", "[[-c], [c]]", "[]"), labels)
    }
}
