package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.parallel.cluster.ClusterSolver
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertTrue

/**
 * A cluster as a solver of arg2p gives the same solutions, graphs and answers as the standard solver with the same
 * theory and flags, in query mode and without: each subclass runs them on a cluster of its own
 */
abstract class ClusterSolverScenarios {
    /** The solver of a cluster with [theory] and [flags] */
    abstract fun cluster(
        theory: String,
        flags: Flags,
    ): ClusterSolver

    /** The flags of the last cluster made set to [flags] */
    abstract fun configure(flags: Flags)

    // The solutions of a goal and the graph of each, as text up to the names of the variables: the arguments with
    // their labels, the attacks
    private data class Solved(
        val solution: String,
        val bindings: Map<String, String?>,
        val arguments: List<String>,
        val attacks: List<String>,
    )

    private fun solved(
        solutions: Solutions,
        graph: () -> Graph,
    ): List<Solved> = solutions.toList().map { solved(it, if (it.isYes) graph() else Graph(emptyList(), emptyList(), emptyList())) }

    private fun solved(
        solution: Solution,
        graph: Graph,
    ) = Solved(
        variant(solution.toString()),
        solution.variables.associateWith { solution.text(it)?.let(::variant) },
        labelled(graph),
        graph.attacks.map { "${it.attacker.identifier} ${it.type} ${it.target.identifier} ${it.on?.identifier}" }.sorted(),
    )

    private fun labelled(graph: Graph) = graph.labellings.map { "${variant(it.argument.descriptor)} ${it.label}" }.sorted()

    // the variables of the terms in [text] (the arguments of a term, the items of a list) all written alike
    private fun variant(text: String) = text.replace(Regex("""(?<=[(\[]|, )[A-Z_][A-Za-z0-9_]*"""), "_")

    // Each goal solved by a cluster of its own and by the standard solver, in query mode and without
    private fun assertSameSolutions(
        theory: String,
        vararg goals: String,
    ) = listOf(Flags.defaults(), Flags.defaults().queryMode(false)).forEach { flags ->
        goals.forEach { goal ->
            val arg2p = Arg2p.of(theory, flags)
            val cluster = cluster(theory, flags)
            assertEquals(solved(arg2p.solve(goal), arg2p::graph), solved(cluster.solve(goal), cluster::graph), "$goal with $flags")
        }
    }

    private val tweety =
        "f1 :=> bird(tweety).\nf2 :=> penguin(tweety).\nr1 : bird(X) => flies(X).\nr2 : penguin(X) => -flies(X).\nsup(r2, r1).\n"

    @Test
    fun tweety() =
        assertSameSolutions(
            tweety,
            "buildLabelSets(In, Out, Und)",
            "answerQuery(flies(tweety), In, Out, Und)",
            "answerQuery(-flies(X), In, Out, Und)",
            "buildLabelSets([In, Out, Und], Arguments)",
        )

    @Test
    fun aPreferenceConflict() =
        assertSameSolutions(
            "r1 : [] => a.\nr2 : [] => -a.\nr3 : a => b.\nr4 : -a => c.\nsup(r1, r2).\n",
            "buildLabelSets(In, Out, Und)",
            "answerQuery(b, In, Out, Und)",
            "answerQuery(c, In, Out, Und)",
        )

    @Test
    fun assumptionBasedArgumentation() =
        assertSameSolutions(
            "graphBuildMode(aba).\nassumption(a).\nassumption(b).\ncontrary(a, x).\ncontrary(b, y).\nr1 : b -> x.\nr2 : [] -> c.\nr3 : c, a -> d.\n",
            "buildLabelSets(In, Out, Und)",
            "answerQuery(d, In, Out, Und)",
        )

    @Test
    fun anAbstractFramework() =
        assertSameSolutions(
            "graphBuildMode(abstract).\na.\nd.\na -> b.\nb -> c.\n",
            "buildLabelSets(In, Out, Und)",
            "answerQuery(c, In, Out, Und)",
        )

    @Test
    fun prologChecksAndComputedValues() =
        assertSameSolutions(
            "old(X) :- X > 65.\nf1 :=> age(anna, 80).\nf2 :=> age(bob, 30).\nr1 : age(P, X), prolog(old(X)) => senior(P).\n" +
                "r2 : age(P, X), prolog(Y is X * 2) => doubled(P, Y).\nr3 : senior(P) => -doubled(P, _).\n",
            "buildLabelSets(In, Out, Und)",
            "answerQuery(senior(P), In, Out, Und)",
            "answerQuery(doubled(P, V), In, Out, Und)",
            "old(80)",
            "old(30)",
            "member(X, [1, 2, 3])",
        )

    @Test
    fun aSemanticsWithSeveralLabellings() {
        val theory = "f1 :=> d.\nf2 :=> -d.\nr1 : d => e.\n"
        val flags = Flags.defaults().argumentLabellingMode(Semantics.PREFERRED)
        // the same labellings, in an order of their own: that of the arguments the workers built
        val expected =
            Arg2p
                .of(theory, flags)
                .graphs()
                .map(::labelled)
                .toSet()
        val graphs = cluster(theory, flags).graphs()
        assertEquals(2, graphs.size)
        assertEquals(expected, graphs.map(::labelled).toSet())
    }

    // the flags changed between goals: the next ones are solved with them, the graph built anew
    @Test
    fun flagsChangedBetweenGoals() {
        val theory = "r1 : [] => a.\nr2 : [] => -a.\nr3 : a => b.\nsup(r1, r2).\n"
        val cluster = cluster(theory, Flags.defaults())
        listOf(
            Flags.defaults().graphExtensions(),
            Flags.defaults().queryMode(false).argumentLabellingMode(Semantics.COMPLETE),
            Flags.defaults(),
        ).forEach { flags ->
            configure(flags)
            listOf("answerQuery(b, In, Out, Und)", "buildLabelSets(In, Out, Und)").forEach { goal ->
                val arg2p = Arg2p.of(theory, flags)
                assertEquals(solved(arg2p.solve(goal), arg2p::graph), solved(cluster.solve(goal), cluster::graph), "$goal with $flags")
            }
        }
    }

    @Test
    fun aQueryIsAnswered() =
        listOf("flies(tweety)", "-flies(tweety)", "bird(X)").forEach {
            assertEquals(Arg2p.of(tweety).answer(it), cluster(tweety, Flags.defaults()).answer(it), it)
        }

    // in query mode, a later goal needing more of the graph explores it further: the graph has what each goal needs,
    // labelled as the standard solver labels it
    @Test
    fun theGraphGrowsWithTheGoals() {
        val theory = "r1 :=> a.\nr2 : a => b.\nr3 :=> c.\nr4 : c => d.\nr5 :=> -d.\n"
        val cluster = cluster(theory, Flags.defaults())
        listOf("b" to setOf("a", "b"), "d" to setOf("a", "b", "c", "d", "-d")).forEach { (statement, built) ->
            val goal = "answerQuery($statement, In, Out, Und)"
            val arg2p = Arg2p.of(theory)
            val expected = solved(arg2p.solve(goal).next(), arg2p.graph())
            val actual = solved(cluster.solve(goal).next(), cluster.graph())
            assertEquals(expected.bindings, actual.bindings, goal)
            val labels = { graph: Graph -> graph.labellings.map { "${it.argument.conclusion} ${it.argument.topRule} ${it.label}" } }
            assertTrue(labels(cluster.graph()).containsAll(labels(arg2p.graph())), goal)
            assertEquals(
                built,
                cluster
                    .graph()
                    .arguments
                    .map { it.conclusion }
                    .toSet(),
                goal,
            )
        }
    }

    // the solutions of goals asked in turn are those each would have alone
    @Test
    fun goalsSolvedInTurn() {
        val cluster = cluster(tweety, Flags.defaults())
        val first = cluster.solve("member(X, [1, 2, 3])")
        assertEquals("1", first.next().text("X"))
        assertEquals("in", cluster.answer("-flies(tweety)").label("-flies(tweety)"))
        assertEquals(listOf("2", "3"), first.toList().filter { it.isYes }.map { it.text("X") })
    }

    @Test
    fun whatTheSolverPrints() {
        val printed = StringBuilder()
        Arg2p
            .builder()
            .theory(tweety)
            .output { printed.append(it) }
            .build()
            .solve("buildLabelSets")
            .next()
        val output = StringBuilder()
        val cluster = cluster(tweety, Flags.defaults()).also { solver -> solver.output = { output.append(it) } }
        assertTrue(cluster.solve("buildLabelSets").next().isYes)
        assertEquals(printed.toString(), output.toString())
    }

    @Test
    fun aGoalThatCannotBeReadIsAnError() {
        val cluster = cluster(tweety, Flags.defaults())
        assertFailsWith<Arg2pException> { cluster.solve("p(") }
        assertTrue(cluster.solve("answerQuery(flies(tweety), In, Out, Und)").next().isYes)
    }
}
