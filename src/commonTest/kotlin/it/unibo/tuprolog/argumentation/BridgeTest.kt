package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.argumentation.bridge.BridgedArgument
import it.unibo.tuprolog.argumentation.bridge.BridgedAttack
import it.unibo.tuprolog.argumentation.bridge.BridgedGraph
import it.unibo.tuprolog.argumentation.bridge.BridgedSolution
import it.unibo.tuprolog.argumentation.bridge.JsBridge
import kotlin.test.Test
import kotlin.test.assertEquals

class BridgeTest {
    @Test
    fun testResolution() {
        JsBridge.solve(
            "buildLabelSets",
            """
            r1 :=> a.
            r2 :=> -a.
            r3 :=> b.
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
                assertEquals(3, res.i.next()?.graph?.arguments?.size ?: 0)
                assertEquals(2, res.i.next()?.graph?.attacks?.size ?: 0)
            }
        }
    }

    @Test
    fun testCausalResolution() {
        JsBridge.solve(
            "causality::evaluate(X, a, b)",
            """
            r1 :=> a.
            r2 : a => b.
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
                assertEquals("yes", res.i.next()?.res ?: "error")
            }
        }
    }

    @Test
    fun jsBridge() {
        val theory =
            """
            d1 : bird(X) => flies(X).
            d2 : penguin(X) => bird(X).
            s1 : penguin(X) -> -flies(X).
            a1 :-> penguin(tweety).
            """.trimIndent()

        val flags =
            """
            graphBuildMode(standard_af).
            statementLabellingMode(statement).
            argumentLabellingMode(grounded_hash).
            orderingPrinciple(last).
            orderingComparator(elitist).
            graphExtension(standardPref).
            queryMode.
            """.trimIndent()

        val res = JsBridge.solve(queryText = "buildLabelSets", theoryText = theory, flagsText = flags, outputConsumer = { print(it) })
        assertEquals(
            res.i.next(),
            BridgedSolution(
                res = "yes",
                query = "buildLabelSets",
                substitutions = emptyArray(),
                exception = "",
                graph =
                    BridgedGraph(
                        arguments =
                            arrayOf(
                                BridgedArgument(id = "A0", descriptor = "A0 : a1 : penguin(tweety)", label = "in"),
                                BridgedArgument(id = "A1", descriptor = "A1 : A0,s1 : '-'(flies(tweety))", label = "in"),
                                BridgedArgument(id = "A2", descriptor = "A2 : A0,d2 : bird(tweety)", label = "in"),
                                BridgedArgument(id = "A3", descriptor = "A3 : A2,d1 : flies(tweety)", label = "out"),
                            ),
                        attacks = arrayOf(BridgedAttack(from = "A1", to = "A3")),
                    ),
            ),
        )
    }
}
