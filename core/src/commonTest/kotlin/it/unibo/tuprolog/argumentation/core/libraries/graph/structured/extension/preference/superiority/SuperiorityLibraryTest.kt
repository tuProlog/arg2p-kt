package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.preference.superiority

import it.unibo.tuprolog.argumentation.core.LibraryTesting
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class SuperiorityLibraryTest {
    // the preferences stated by the theory
    private fun module(
        principle: String = "last",
        comparator: String = "elitist",
        preferences: String = "sup(r1, r2).\nsup(f1, f2).",
    ) = LibraryTesting("orderingPrinciple($principle).\norderingComparator($comparator).\n$preferences")

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
        assertTrue(module.holds("contextCheck(superior([[r1], [r1], []], [[r2], [r2], []], _, true))"))
        assertFalse(module.holds("superiority::superiorArgument($r2, $r1)"))
        assertTrue(module.holds("contextCheck(superior([[r2], [r2], []], [[r1], [r1], []], _, false))"))
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
        val module = module(preferences = "sup(r1, r2).\nsup(f1, f2).\nsup(r1, r3).")
        val r2r3 = argument("[[r2, r3], [r2, r3], []]")
        // elitist: r2 and r3 are each inferior to r1, by a preference of its own
        assertEquals(setOf("[sup(r1, r2)]", "[sup(r1, r3)]"), module.values("superiority::superiorArgument($r1, $r2r3, P)", "P").toSet())
        // a strict argument needs no preference
        assertEquals(listOf("[]"), module.values("superiority::superiorArgument(${argument("[[], [], []]")}, $r2, P)", "P"))
    }

    @Test
    fun theStatedPreferencesAreClosedTransitively() {
        val module = LibraryTesting("sup(r1, r2).\nsup(r2, r3).\nsup(r3, r4).\nsup(s1, s2).\nsup(s2, s1).")
        assertEquals(
            setOf("r1, r2", "r2, r3", "r3, r4", "r1, r3", "r2, r4", "r1, r4", "s1, s2", "s2, s1").map { "sup($it)" }.toSet(),
            module.values("contextReset, superiority::preference(X, Y), S = sup(X, Y)", "S").toSet(),
        )
    }
}
