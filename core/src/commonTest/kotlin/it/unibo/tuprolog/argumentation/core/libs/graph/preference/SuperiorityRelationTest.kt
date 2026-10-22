package it.unibo.tuprolog.argumentation.core.libs.graph.preference

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class SuperiorityRelationTest {
    private fun module(
        principle: String = "last",
        comparator: String = "elitist",
    ) = ModuleTesting("orderingPrinciple($principle).\norderingComparator($comparator).").also {
        it.run("context_assert(sup(r1, r2))", "context_assert(sup(f1, f2))")
    }

    private fun argument(info: String) = "[[x], x, [x], [], $info]"

    private val r1 = argument("[[r1], [r1], []]")
    private val r2 = argument("[[r2], [r2], []]")

    @Test
    fun aPreferredRuleMakesItsArgumentSuperior() {
        val module = module()
        assertTrue(module.holds("superiority::superiorArgument($r1, $r2)"))
        assertFalse(module.holds("superiority::superiorArgument($r2, $r1)"))
        assertEquals("[sup(r1, r2)]", module.value("superiority::superiorArgument($r1, $r2, P)", "P"))
    }

    @Test
    fun premisesAreComparedWithoutDefeasibleRules() {
        val module = module()
        assertTrue(module.holds("superiority::superiorArgument(${argument("[[], [], [f1]]")}, ${argument("[[], [], [f2]]")})"))
    }

    @Test
    fun aStrictArgumentIsSuperiorToADefeasibleOne() {
        val module = module()
        assertTrue(module.holds("superiority::superiorArgument(${argument("[[], [], []]")}, $r2)"))
        assertFalse(module.holds("superiority::superiorArgument($r2, ${argument("[[], [], []]")})"))
    }

    @Test
    fun theComparatorsWeighSetsOfRules() {
        val r2r3 = argument("[[r2, r3], [r2, r3], []]")
        // elitist: some rule of the weaker set is inferior to all those of the other one
        val elitist = module()
        assertTrue(elitist.holds("superiority::superiorArgument($r1, $r2r3)"))
        // democrat: each rule of the weaker set is inferior to some rule of the other one
        val democrat = module(comparator = "democrat")
        assertFalse(democrat.holds("superiority::superiorArgument($r1, $r2r3)"))
        assertTrue(democrat.holds("superiority::superiorArgument($r1, $r2)"))
    }

    @Test
    fun aComparisonIsMadeOnce() {
        val module = module()
        module.run("superiority::superiorArgument($r1, $r2)")
        assertTrue(module.holds("context_check(superior([[r1], [r1], []], [[r2], [r2], []], _, true))"))
        assertFalse(module.holds("superiority::superiorArgument($r2, $r1)"))
        assertTrue(module.holds("context_check(superior([[r2], [r2], []], [[r1], [r1], []], _, false))"))
    }
}
