package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.argumentation.bridge.BridgedArgument
import it.unibo.tuprolog.argumentation.bridge.BridgedAttack
import it.unibo.tuprolog.argumentation.bridge.BridgedGraph
import it.unibo.tuprolog.argumentation.bridge.BridgedSolution
import it.unibo.tuprolog.argumentation.bridge.JsBridge
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class BridgeTest {
    @Test
    fun testNaturalResolution() {
        JsBridge
            .solve(
                "buildLabelSets",
                """
                "rule on X" : "X is a cat", ~("X is well trained") => "X is annoying". 

                f :=> "fuffi is a cat".
                f1 : [] => -"fuffi is annoying".
                f2 :=> "fuffi is well trained".
                f2 : standard_fact => -"rule on fuffi".
                f3 :=> standard_fact. 
                f4 :=> -"[pippo] is a dog".
                f5 :=> "pino is a whale".
                
                """.trimIndent(),
                """
                graphBuildMode(standard_af).
                statementLabellingMode(statement).
                argumentLabellingMode(grounded).
                orderingPrinciple(last).
                orderingComparator(elitist).
                graphExtension(standardPref).
                queryMode.
                naturalTerms.
                """.trimIndent(),
                { println(it) },
            ).let { res ->
                val graph = res.i.next()?.graph
                assertEquals(8, graph?.arguments?.size ?: 0)
                assertEquals(4, graph?.attacks?.size ?: 0)
            }
    }

    @Test
    fun testResolution() {
        JsBridge
            .solve(
                "buildLabelSets",
                """
                r1 :=> a.
                r2 :=> b.
                r3 :=> c.
                
                conflict([a], [b]).
                conflict([b], [a]).
                """.trimIndent(),
                """
                graphBuildMode(standard_af).
                statementLabellingMode(statement).
                argumentLabellingMode(grounded).
                orderingPrinciple(last).
                orderingComparator(elitist).
                graphExtension(standardPref).
                queryMode.
                """.trimIndent(),
                { println(it) },
            ).let { res ->
                val graph = res.i.next()?.graph
                assertEquals(3, graph?.arguments?.size ?: 0)
                assertEquals(2, graph?.attacks?.size ?: 0)
            }
    }

    @Test
    fun testCausalResolution() {
        JsBridge
            .solve(
                "causality::ness(X, a, b)",
                """
                r1 :=> a.
                r2 : a => b.
                """.trimIndent(),
                """
                graphBuildMode(standard_af).
                statementLabellingMode(statement).
                argumentLabellingMode(grounded).
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
            argumentLabellingMode(grounded).
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

    @Test
    fun testContextExploration() {
        val res =
            JsBridge.solve(
                "buildLabelSets",
                """
                r1 :=> a.
                r2 : a => b.
                r3 :=> -b.
                sup(r3, r2).
                """.trimIndent(),
                """
                graphBuildMode(standard_af).
                statementLabellingMode(statement).
                argumentLabellingMode(grounded).
                orderingPrinciple(last).
                orderingComparator(elitist).
                graphExtension(standardPref).
                """.trimIndent(),
                { },
            )
        val solution = res.i.next()!!
        // every stage of the computation is kept: the first context is empty, the last one is the mined graph
        assertTrue(solution.context > 0)
        assertEquals(0, res.graphAt(0).arguments.size)
        assertEquals(solution.graph, res.graphAt(solution.context))
        // the attack from -b to b is in the built graph, and removed by the preference (r3 is superior to r2)
        assertTrue((0 until solution.context).any { res.graphAt(it).attacks.size > solution.graph.attacks.size })
        // buffered information can be queried as well, by the names of the goal variables
        val statements = res.queryContext(solution.context, "statIn(S)").map { row -> row.single { it.first == "S" }.second }
        assertEquals(setOf("[a]", "[- b]"), statements.toSet())
    }
}
