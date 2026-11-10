package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.parallel.GoalSolver
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertTrue

/**
 * A parallel solver as a solver of arg2p gives the same solutions, graphs and answers as the standard solver with the same
 * theory and flags, in query mode and without: each subclass runs them on a parallel solver of its own
 */
abstract class GoalSolverScenarios {
    /** The client solver with [theory] and [flags] */
    abstract fun parallel(
        theory: String,
        flags: Flags,
    ): GoalSolver

    /** The flags of the last parallel solver made set to [flags] */
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

    // Each goal solved by a parallel solver of its own and by the standard solver, in query mode and without
    private fun assertSameSolutions(
        theory: String,
        vararg goals: String,
    ) = listOf(Flags.defaults(), Flags.defaults().queryMode(false)).forEach { flags ->
        goals.forEach { goal ->
            val arg2p = Arg2p.of(theory, flags)
            val parallel = parallel(theory, flags)
            assertEquals(solved(arg2p.solve(goal), arg2p::graph), solved(parallel.solve(goal), parallel::graph), "$goal with $flags")
        }
    }

    private val tweety =
        "f1 :=> bird(tweety).\nf2 :=> penguin(tweety).\nr1 : bird(X) => flies(X).\nr2 : penguin(X) => -flies(X).\nsup(r2, r1).\n"

    @Test
    fun tweety() =
        assertSameSolutions(
            tweety,
            "solve(In, Out, Und)",
            "solve(flies(tweety), In, Out, Und)",
            "solve(-flies(X), In, Out, Und)",
            "solve(In, Out, Und), store::argumentLabelling(ArgsIn, ArgsOut, ArgsUnd)",
        )

    @Test
    fun aPreferenceConflict() =
        assertSameSolutions(
            "r1 : [] => a.\nr2 : [] => -a.\nr3 : a => b.\nr4 : -a => c.\nsup(r1, r2).\n",
            "solve(In, Out, Und)",
            "solve(b, In, Out, Und)",
            "solve(c, In, Out, Und)",
        )

    @Test
    fun assumptionBasedArgumentation() =
        assertSameSolutions(
            "graphBuildMode(aba).\nassumption(a).\nassumption(b).\ncontrary(a, x).\ncontrary(b, y).\nr1 : b -> x.\nr2 : [] -> c.\nr3 : c, a -> d.\n",
            "solve(In, Out, Und)",
            "solve(d, In, Out, Und)",
        )

    @Test
    fun anAbstractFramework() =
        assertSameSolutions(
            "graphBuildMode(abstract).\na.\nd.\na -> b.\nb -> c.\n",
            "solve(In, Out, Und)",
            "solve(c, In, Out, Und)",
        )

    @Test
    fun prologChecksAndComputedValues() =
        assertSameSolutions(
            "old(X) :- X > 65.\nf1 :=> age(anna, 80).\nf2 :=> age(bob, 30).\nr1 : age(P, X), prolog(old(X)) => senior(P).\n" +
                "r2 : age(P, X), prolog(Y is X * 2) => doubled(P, Y).\nr3 : senior(P) => -doubled(P, _).\n",
            "solve(In, Out, Und)",
            "solve(senior(P), In, Out, Und)",
            "solve(doubled(P, V), In, Out, Und)",
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
        val graphs = parallel(theory, flags).graphs()
        assertEquals(2, graphs.size)
        assertEquals(expected, graphs.map(::labelled).toSet())
    }

    // the flags changed between goals: the next ones are solved with them, the graph built anew
    @Test
    fun flagsChangedBetweenGoals() {
        val theory = "r1 : [] => a.\nr2 : [] => -a.\nr3 : a => b.\nsup(r1, r2).\n"
        val parallel = parallel(theory, Flags.defaults())
        listOf(
            Flags.defaults().graphExtensions(),
            Flags.defaults().queryMode(false).argumentLabellingMode(Semantics.COMPLETE),
            Flags.defaults(),
        ).forEach { flags ->
            configure(flags)
            listOf("solve(b, In, Out, Und)", "solve(In, Out, Und)").forEach { goal ->
                val arg2p = Arg2p.of(theory, flags)
                assertEquals(solved(arg2p.solve(goal), arg2p::graph), solved(parallel.solve(goal), parallel::graph), "$goal with $flags")
            }
        }
    }

    @Test
    fun aQueryIsAnswered() =
        listOf("flies(tweety)", "-flies(tweety)", "bird(X)").forEach {
            assertEquals(Arg2p.of(tweety).answer(it), parallel(tweety, Flags.defaults()).answer(it), it)
        }

    // in query mode, each goal has a graph of its own, with what it needs, labelled as the standard solver labels it
    @Test
    fun theGraphIsBuiltForEachGoal() {
        val theory = "r1 :=> a.\nr2 : a => b.\nr3 :=> c.\nr4 : c => d.\nr5 :=> -d.\n"
        val parallel = parallel(theory, Flags.defaults())
        listOf("b" to setOf("a", "b"), "d" to setOf("c", "d", "- d")).forEach { (statement, built) ->
            val goal = "solve($statement, In, Out, Und)"
            val arg2p = Arg2p.of(theory)
            val expected = solved(arg2p.solve(goal).next(), arg2p.graph())
            val actual = solved(parallel.solve(goal).next(), parallel.graph())
            assertEquals(expected.bindings, actual.bindings, goal)
            val labels = { graph: Graph -> graph.labellings.map { "${it.argument.conclusion} ${it.argument.topRule} ${it.label}" } }
            assertEquals(labels(arg2p.graph()).toSet(), labels(parallel.graph()).toSet(), goal)
            assertEquals(
                built,
                parallel
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
        val parallel = parallel(tweety, Flags.defaults())
        val first = parallel.solve("member(X, [1, 2, 3])")
        assertEquals("1", first.next().text("X"))
        assertEquals("in", parallel.answer("-flies(tweety)").single().label("-flies(tweety)"))
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
            .solve("solve")
            .next()
        val output = StringBuilder()
        val parallel = parallel(tweety, Flags.defaults()).also { solver -> solver.output = { output.append(it) } }
        assertTrue(parallel.solve("solve").next().isYes)
        assertEquals(printed.toString(), output.toString())
    }

    @Test
    fun aGoalThatCannotBeReadIsAnError() {
        val parallel = parallel(tweety, Flags.defaults())
        assertFailsWith<Arg2pException> { parallel.solve("p(") }
        assertTrue(parallel.solve("solve(flies(tweety), In, Out, Und)").next().isYes)
    }
}
