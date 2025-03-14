package it.unibo.tuprolog.argumentation.core.system

import it.unibo.tuprolog.argumentation.core.bridge.JsBridge
import kotlin.test.Test

class SolvingTest {
    @Test
    fun testResolution() {
        JsBridge.solve(
            "sister(X, Y)",
            """
            sister(jane, paul).
            sister(paul, jane).
            """.trimIndent(),
            """
            graphBuildMode(standard_af).
            statementLabellingMode(statement).
            argumentLabellingMode(grounded_hash).
            orderingPrinciple(last).
            orderingComparator(elitist).
            graphExtension(standardPref).
            queryMode.
            """.trimIndent(),
            { println(it) },
        ).let { res ->
            if (res.i.hasNext()) {
                println(res.i.next()?.substitutions)
            }
        }
    }
}
