package it.unibo.tuprolog.argumentation.core.modules.graph.extension

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class SuperiorityModuleTest {
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

    // normal is defined on the last rules: it does not go with the weakest link
    @Test
    fun theNormalComparatorNeedsTheLastLink() {
        assertTrue(module(comparator = "normal").holds("superiority::superiorArgument($r1, $r2)"))
        val weakest = module(principle = "weakest", comparator = "normal")
        assertTrue(
            weakest.holds(
                "catch(superiority::superiorArgument($r1, $r2), error(domain_error(orderingPrinciple(last), weakest), _), true)",
            ),
        )
    }

    @Test
    fun everyJustificationIsGiven() {
        val module = module().also { it.run("context_assert(sup(r1, r3))") }
        val r2r3 = argument("[[r2, r3], [r2, r3], []]")
        // elitist: r2 and r3 are each inferior to r1, by a preference of its own
        assertEquals(setOf("[sup(r1, r2)]", "[sup(r1, r3)]"), module.values("superiority::superiorArgument($r1, $r2r3, P)", "P").toSet())
        // a strict argument needs no preference
        assertEquals(listOf("[]"), module.values("superiority::superiorArgument(${argument("[[], [], []]")}, $r2, P)", "P"))
    }
}
