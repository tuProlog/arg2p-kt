package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.argumentation.core.model.CompactGraph
import it.unibo.tuprolog.argumentation.core.model.Graph
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class CompactGraphTest {
    private val theory =
        """
        f1 :=> a.
        f2 :=> -a.
        r1 : a => b.
        r2 : b => c.
        f3 :=> d.
        r3 : d => -c.
        sup(r3, r2).
        """.trimIndent()

    private fun assertSame(
        graph: Graph,
        compact: CompactGraph,
    ) {
        assertEquals(graph.arguments.size, compact.size)
        val at = (0 until compact.size).associateBy { compact.identifier(it) }
        graph.arguments.forEach { argument ->
            val p = at.getValue(argument.identifier)
            val made = compact.argument(p)
            assertEquals(argument.conclusion, made.conclusion)
            assertEquals(argument.topRule, made.topRule)
            assertEquals(argument.rules, made.rules)
            assertEquals(argument.groundings, made.groundings)
            assertEquals(argument.defeasibleRules, made.defeasibleRules)
            assertEquals(argument.defeasiblePremises, made.defeasiblePremises)
            assertEquals(argument.lastDefeasibleRules, made.lastDefeasibleRules)
            assertEquals(argument.descriptor, compact.descriptor(p))
        }
        val named = { p: Int -> compact.identifier(p) }
        assertEquals(
            graph.labellings.map { it.argument.identifier to it.label }.sortedBy { it.first },
            (0 until compact.size).map { named(it) to compact.label(it) }.sortedBy { it.first },
        )
        assertEquals(
            graph.attacks
                .map {
                    listOf(
                        it.attacker.identifier,
                        it.target.identifier,
                        it.on?.identifier,
                        it.type,
                    )
                }.sortedBy { it.toString() },
            compact.attackers.indices
                .map {
                    listOf(
                        named(compact.attackers[it]),
                        named(compact.targets[it]),
                        named(compact.on[it]),
                        compact.attackTypes[compact.attackTypeOf[it]],
                    )
                }.sortedBy { it.toString() },
        )
        assertEquals(
            graph.supports.map { it.supporter.identifier to it.supported.identifier }.sortedBy { it.toString() },
            compact.supporters.indices
                .map { named(compact.supporters[it]) to named(compact.supported[it]) }
                .sortedBy { it.toString() },
        )
    }

    @Test
    fun theGraphOfALabelling() {
        val arg2p = Arg2p.of(theory, Flags.defaults().queryMode(false))
        val graph = arg2p.graphs().single()
        assertTrue(graph.attacks.isNotEmpty())
        assertSame(graph, arg2p.compactGraphs().single())
    }

    @Test
    fun theGraphOfEachLabelling() {
        val arg2p = Arg2p.of(theory, Flags.defaults().queryMode(false).argumentLabellingMode(Semantics.PREFERRED))
        val graphs = arg2p.graphs()
        val compact = arg2p.compactGraphs()
        assertEquals(graphs.size, compact.size)
        assertTrue(graphs.size > 1)
        graphs.zip(compact).forEach { (graph, arrays) -> assertSame(graph, arrays) }
    }

    @Test
    fun theGraphOfABranch() {
        val arg2p = Arg2p.of(theory)
        arg2p.answer("c")
        assertSame(arg2p.graphAt(arg2p.activeBranch), arg2p.compactGraphAt(arg2p.activeBranch))
    }

    @Test
    fun theObjectsOfACompactGraph() {
        val arg2p = Arg2p.of(theory, Flags.defaults().queryMode(false))
        val graph = arg2p.graphs().single()
        val made = arg2p.compactGraphs().single().toGraph()
        assertEquals(graph.arguments.map { it.descriptor }, made.arguments.map { it.descriptor })
        assertEquals(
            graph.labellings.map { it.argument.identifier to it.label },
            made.labellings.map { it.argument.identifier to it.label },
        )
        assertEquals(graph.attacks.map { it.toString() }.sorted(), made.attacks.map { it.toString() }.sorted())
        assertEquals(graph.supports.map { it.toString() }.sorted(), made.supports.map { it.toString() }.sorted())
    }

    @Test
    fun theGraphOfTheLastResolution() {
        val arg2p = Arg2p.of(theory)
        arg2p.answer("c")
        assertSame(arg2p.graph(), arg2p.compactGraph())
    }
}
