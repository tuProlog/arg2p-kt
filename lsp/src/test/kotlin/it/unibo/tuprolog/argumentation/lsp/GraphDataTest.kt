package it.unibo.tuprolog.argumentation.lsp

import it.unibo.tuprolog.argumentation.Arg2p
import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.model.Graph
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/** The graphs the pages read, made from the arrays of a compact graph: what the objects of the API tell */
class GraphDataTest {
    // The graph as the objects of the API tell it
    private fun told(graph: Graph) =
        GraphData(
            "profile",
            graph.labellings.map { (argument, label) ->
                ArgumentData(
                    argument.identifier,
                    argument.descriptor,
                    argument.conclusion,
                    argument.topRule,
                    label,
                    argument.supports.map { it.identifier },
                )
            },
            graph.attacks.map { AttackData(it.attacker.identifier, it.target.identifier, it.type, it.on?.identifier) },
        )

    private fun assertSame(solver: Arg2p) {
        val graph = told(solver.graphAt(solver.activeBranch))
        val data = GraphData.of(solver.compactGraphAt(solver.activeBranch), "profile")
        assertEquals(graph.arguments, data.arguments)
        assertEquals(graph.attacks.toSet(), data.attacks.toSet())
        assertEquals(graph.attacks.size, data.attacks.size)
    }

    // Each solution of the labellings of [theory], and that there is an argument in the graph of each
    private fun assertSame(
        theory: String,
        flags: Flags = Flags.defaults(),
    ) {
        val solver = Arg2p.of(theory, flags)
        val solutions = solver.solve("arg2p::solve(In, Out, Und)")
        var graphs = 0
        while (solutions.hasNext()) {
            if (!solutions.next().isYes) continue
            assertSame(solver)
            assertTrue(solver.graph().arguments.isNotEmpty())
            graphs++
        }
        assertTrue(graphs > 0, "no labelling of $theory")
    }

    @Test
    fun aGraphOfRulesAttacksAndSupports() =
        assertSame(
            """
            f1 :=> bird(tweety).
            f2 :=> penguin(tweety).
            r1 : bird(X) => flies(X).
            r2 : penguin(X) => -flies(X).
            r3 : flies(X) => moves(X).
            r2 > r1.
            """.trimIndent(),
        )

    @Test
    fun theGraphOfTheWholeTheory() =
        assertSame(
            """
            f1 :=> a.
            r1 : a => b.
            r2 : a => -b.
            r3 : b => c.
            r4 : -b => -c.
            """.trimIndent(),
            Flags.defaults().queryMode(false),
        )

    @Test
    fun anAbstractFramework() =
        assertSame(
            """
            graphBuildMode(abstract).
            a. b. c.
            a -> b.
            b -> c.
            c -> a.
            """.trimIndent(),
        )

    @Test
    fun aGraphOfNoAttack() = assertSame("f1 :=> a.")

    @Test
    fun everyLabellingOfASemanticsOfSeveral() =
        assertSame(
            """
            argumentLabellingMode(preferred).
            r1 :=> a.
            r2 :=> -a.
            r3 : a => b.
            """.trimIndent(),
        )

    @Test
    fun theGraphOfAQuery() {
        val solver = Arg2p.of("f1 :=> a.\nr1 : a => b.\nr2 : a => -b.")
        solver.answer("b")
        assertSame(solver)
        assertTrue(solver.graph().arguments.isNotEmpty())
    }

    // A goal of Prolog builds no graph, as the pages are told by the arrays as by the objects
    @Test
    fun aGoalOfPrologBuildsTheSameGraph() {
        val solver = Arg2p.of("f1 :=> a.")
        val solutions = solver.solve("X = 1")
        solutions.next()
        val graph = runCatching { told(solver.graphAt(solver.activeBranch)) }
        val data = runCatching { GraphData.of(solver.compactGraphAt(solver.activeBranch), "profile") }
        assertEquals(graph.isSuccess, data.isSuccess, "objects: $graph, arrays: $data")
        if (graph.isSuccess) assertEquals(graph.getOrThrow(), data.getOrThrow())
    }
}
