package it.unibo.tuprolog.argumentation.core.js

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

// What JS does with arg2p: solvers described by their theory and flags, their solutions, and their graphs
class JsArg2pTest {
    private val flags =
        """
        statementLabellingMode(statement).
        argumentLabellingMode(grounded).
        orderingPrinciple(last).
        orderingComparator(elitist).
        graphExtension(standardPref).
        queryMode.
        """.trimIndent()

    private fun solver(
        theory: String,
        flags: String = this.flags,
    ) = JsSolverBuilder()
        .theory(theory)
        .flags(flags)
        .output { }
        .build()

    @Test
    fun theGraphOfASolution() {
        val solver =
            solver(
                """
                d1 : bird(X) => flies(X).
                d2 : penguin(X) => bird(X).
                s1 : penguin(X) -> -flies(X).
                a1 :-> penguin(tweety).
                autoTransposition(off).
                """.trimIndent(),
            )
        val solution = JsArg2p.solve(solver, "buildLabelSets").next()
        assertTrue(solution.isYes)
        assertEquals("buildLabelSets", solution.query)
        assertEquals(
            JsGraph(
                arrayOf(
                    JsArgument("A0", "A0 : a1 : penguin(tweety)", "in"),
                    JsArgument("A1", "A1 : A0,s1 : '-'(flies(tweety))", "in"),
                    JsArgument("A2", "A2 : A0,d2 : bird(tweety)", "in"),
                    JsArgument("A3", "A3 : A2,d1 : flies(tweety)", "out"),
                ),
                arrayOf(JsAttack("A1", "A3", "rebut", "A3")),
            ),
            JsArg2p.graph(solver),
        )
    }

    @Test
    fun conflictsStatedByTheTheory() {
        val solver = solver("r1 :=> a.\nr2 :=> b.\nr3 :=> c.\n\nconflict([a], [b]).\nconflict([b], [a]).")
        JsArg2p.solve(solver, "buildLabelSets").next()
        val graph = JsArg2p.graph(solver)
        assertEquals(3, graph.arguments.size)
        assertEquals(2, graph.attacks.size)
    }

    @Test
    fun naturalTerms() {
        val solver =
            solver(
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
                "$flags\nnaturalTerms.",
            )
        JsArg2p.solve(solver, "buildLabelSets").next()
        val graph = JsArg2p.graph(solver)
        assertEquals(8, graph.arguments.size)
        assertEquals(4, graph.attacks.size)
    }

    @Test
    fun theStagesOfTheComputation() {
        val solver = solver("r1 :=> a.\nr2 : a => b.\nr3 :=> -b.\nsup(r3, r2).", flags.replace("queryMode.", ""))
        JsArg2p.solve(solver, "buildLabelSets").next()
        val last = JsArg2p.activeBranch(solver)
        val graph = JsArg2p.graph(solver)
        // every stage is kept: the first branch is empty, the last one has the mined graph
        assertTrue(last > 0)
        assertEquals(0, JsArg2p.graphAt(solver, 0).arguments.size)
        assertEquals(graph, JsArg2p.graphAt(solver, last))
        // the attack from -b to b is in the built graph, and removed by the preference (r3 is superior to r2)
        assertTrue((0 until last).any { JsArg2p.graphAt(solver, it).attacks.size > graph.attacks.size })
        // what the branches store can be queried as well
        val statements = JsArg2p.contextFacts(solver, last, "statIn(S)")
        val found = generateSequence { if (statements.hasNext()) statements.next().formatted("S") else null }.toSet()
        assertEquals(setOf("[a]", "[- b]"), found)
    }

    @Test
    fun theDefaultFlagsAndTheOutput() {
        val printed = StringBuilder()
        val solver = JsSolverBuilder().theory("r1 :=> a.").output { printed.append(it) }.build()
        val solution = JsArg2p.solve(solver, "answerQuery(a, Yes, No, Und), write(done)").next()
        assertEquals("[a]", solution.formatted("Yes"))
        assertEquals("done", printed.toString())
    }
}
