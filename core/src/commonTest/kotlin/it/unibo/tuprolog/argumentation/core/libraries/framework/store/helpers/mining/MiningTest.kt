package it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.mining

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/** The arguments mined from a graph, identified in order of their number of sub-arguments first */
class MiningTest {
    private val config =
        """
        statementLabellingMode(statement).
        argumentLabellingMode(grounded).
        orderingPrinciple(last).
        orderingComparator(elitist).
        graphExtension(standardPref).
        """.trimIndent()

    @Test
    fun argumentsWithFewerSubArgumentsComeFirst() {
        // b and c have three rules each, but c rests on one sub-argument (y), b on two (p and q)
        val theory = "f1 :=> p.\nf2 :=> q.\nr1 : p, q => b.\nr2 : [] => x.\nr3 : x => y.\nr4 : y => c.\n$config"
        val solver = solverWithTheory(theory)
        val solution = solver.solve(Struct.parse("solve(_, _, _), contextActive(B)", solver.operators)).first()
        val active = solution.substitution.entries.first { it.key.name == "B" }
        val branch =
            active.value
                .castToInteger()
                .intValue
                .toInt()
        val order = solver.arguments(branch).map { it.conclusion }
        assertTrue(order.indexOf("c") < order.indexOf("b"), "$order")
    }

    // holdsAt(f, T) for T > 1 and its instance holdsAt(f, 7), which -holdsAt(f, 7) attacks: both OUT, each with its
    // own attack, the open argument not taken for its instance (they unify); its pieces before and after 7 IN
    @Test
    fun anArgumentForAnOpenStatementIsNotItsInstance() {
        val theory = "f1 :=> s.\nr0 : s, {T > 1} => holdsAt(f, T).\nc1 :=> -holdsAt(f, 7).\ngraphExtension(constraints).\n$config"
        val solver = solverWithTheory(theory)
        solver.solve(Struct.parse("solve(_, _, _)", solver.operators)).first()
        val graph = solver.graph()
        val holds = graph.labellings.filter { it.argument.conclusion.startsWith("holdsAt") }
        assertEquals(listOf("in", "in", "out", "out"), holds.map { it.label }.sorted(), "$holds")
        assertEquals(
            2,
            graph.attacks
                .map { it.target }
                .toSet()
                .size,
            "${graph.attacks}",
        )
    }
}
