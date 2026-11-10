package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.argument.argumentSearch

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

// The search steered by a query finds the arguments the whole graph has: no more for what the query binds
class ArgumentSearchLibraryTest {
    // The answers to Query as In/Out/Und lists, in query mode and on the whole graph
    private fun answers(
        theory: String,
        query: String,
    ): List<String> =
        listOf(true, false).map { queryMode ->
            val solver = Arg2pSolverFactory.default(theory = theory, settings = Flags.defaults().queryMode(queryMode).library())
            val solution = solver.solve(Struct.parse("solve($query, I, O, U)", solver.operators)).first()
            listOf("I", "O", "U").joinToString(" ") { solution.substitution.getByName(it).toString() }
        }

    // p(X) holds for no value in particular: without an extension allowing open conclusions, no argument, the query
    // binding X or not
    @Test
    fun aPremiseWithVariablesNeedsAnExtension() {
        val none = "[] [] []"
        assertEquals(listOf(none, none), answers("f1 :=> p(X).\n", "p(a)"))
    }

    // a premise of the query's rule bound by it as the whole graph binds it: the same answers
    @Test
    fun theQueryBindsWhatTheGraphBinds() {
        val theory = "f1 :=> q(a).\nf2 :=> q(b).\nr : q(X) => p(X).\nc :=> -q(b).\n"
        val (query, whole) = answers(theory, "p(a)")
        assertEquals(whole, query)
        assertEquals("[p(a)] [] []", query)
    }
}
