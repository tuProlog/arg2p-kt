package it.unibo.tuprolog.argumentation.core.system

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.mining.graph
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

// A rule may use the same argument for two of its premises: the argument is built, in the whole graph as in query mode,
// and read back with its one support
class SharedSupportTest {
    private val theories =
        listOf(
            "f :-> p(a).\nr : p(X), p(Y) -> q(X, Y).\n",
            "f :=> p(a).\nr : p(X), p(Y) => q(X, Y).\n",
        )

    private fun inStatements(
        theory: String,
        goal: String,
        flags: Flags,
    ) = Arg2pSolverFactory.default(theory = theory, settings = flags.library()).let { solver ->
        val solution = solver.solve(Struct.parse(goal, solver.operators)).first()
        solution.substitution.entries
            .first { it.key.name == "In" }
            .value
            .toString() to solver.graph()
    }

    @Test
    fun theWholeGraphHasTheArgument() {
        for (theory in theories) {
            val (statements, graph) = inStatements(theory, "solve(In, Out, Und)", Flags.defaults().queryMode(false))
            assertTrue("q(a, a)" in statements, statements)
            val built = graph.arguments.single { it.conclusion == "q(a, a)" }
            assertEquals(1, graph.supports.count { it.supported == built }, "one support")
            assertEquals("in", graph.labellings.single { it.argument == built }.label)
        }
    }

    @Test
    fun queryModeHasTheSameArgument() {
        for (theory in theories) {
            val (statements, graph) = inStatements(theory, "solve(q(a, a), In, Out, Und)", Flags.defaults())
            assertTrue("q(a, a)" in statements, statements)
            assertEquals(1, graph.arguments.count { it.conclusion == "q(a, a)" })
        }
    }
}
