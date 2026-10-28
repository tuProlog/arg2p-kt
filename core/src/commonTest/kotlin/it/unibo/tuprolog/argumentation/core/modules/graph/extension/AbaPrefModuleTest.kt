package it.unibo.tuprolog.argumentation.core.modules.graph.extension

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.random.Random
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

/**
 * Flat ABA and ABA+ on the arguments of arg2p (the profiles aba and aba_plus, the extension abaPref) against the
 * definitions on sets of assumptions (see [AbaReference]): the statements labelled in are those the extensions derive,
 * for every semantics. ABA+ on the theories satisfying Weak Contraposition, the others refused.
 */
class AbaPrefModuleTest {
    private fun solutions(theory: String) =
        Arg2pSolverFactory.default(theory = theory, settings = FlagsBuilder().create()).let { solver ->
            solver.solve(Struct.parse("buildLabelSets(I, _, _)", solver.operators)).toList()
        }

    // The statements labelled in by each labelling of arg2p
    private fun labelled(theory: String): Set<Set<String>> {
        val solutions = solutions(theory).filter { it.isYes }
        check(solutions.isNotEmpty()) { "No labelling\n$theory" }
        return solutions
            .map { solution ->
                solution.substitution
                    .getByName("I")!!
                    .castToList()
                    .toList()
                    .map {
                        it
                            .castToList()
                            .toList()
                            .single()
                            .toString()
                    }.toSet()
            }.toSet()
    }

    private fun refused(theory: String) = solutions(theory).first().isHalt

    // Example 13 of Čyras and Toni: {beta, beta2} attacks eps, though beta is weaker than it; the attack is reversed
    // on the set, so that eps also attacks the argument for the contrary of alpha resting on the same set
    @Test
    fun anAttackIsReversedOnEveryArgumentOfTheSameAssumptions() {
        val framework =
            AbaReference(
                listOf("alpha", "beta", "beta2", "eps"),
                listOf(
                    "neps" to listOf("beta", "beta2"),
                    "nbeta" to listOf("eps", "beta2"),
                    "nbeta2" to listOf("eps", "beta"),
                    "nbeta" to listOf("beta"),
                    "nbeta2" to listOf("beta2"),
                    "nalpha" to listOf("beta", "beta2"),
                    "nbeta" to listOf("alpha", "beta2"),
                    "nbeta2" to listOf("alpha", "beta"),
                ),
                mapOf("alpha" to "nalpha", "beta" to "nbeta", "beta2" to "nbeta2", "eps" to "neps"),
                setOf("beta" to "eps"),
            )
        assertTrue(framework.weakContraposition)
        assertEquals(setOf("alpha", "eps"), framework.grounded)
        assertEquals(setOf(framework.grounded), labelled(framework.theory("aba_plus")))
    }

    // {a0, a1} derives p1, the contrary of a1 and of the stronger a2: a2 attacks {a0, a1} in reverse, a1 stands. The
    // contraposed attack of a1 on a0 alone would not defend it: {a0, a1} attacks a1 back
    @Test
    fun aReversedAttackGroundsTheDefenceOfTheContraposition() {
        val framework =
            AbaReference(
                listOf("a0", "a1", "a2", "a3"),
                listOf("p0" to listOf("a0", "a1"), "p1" to listOf("a0", "a1")),
                mapOf("a0" to "a1", "a1" to "p1", "a2" to "p1", "a3" to "p0"),
                setOf("a0" to "a3", "a1" to "a2", "a3" to "a2", "a0" to "a2"),
            )
        assertTrue(framework.weakContraposition)
        assertEquals(setOf("a1", "a2", "a3"), framework.grounded)
        assertEquals(setOf(framework.grounded), labelled(framework.theory("aba_plus")))
    }

    // {a0, a1} derives x, the contrary of a0, a1 weaker than a0; nothing derives y, the contrary of a1, from a0: the
    // sets {a0} and {a1} stand, not together, though no argument attacks another. Refused
    @Test
    fun aTheoryWithNoWeakContrapositionIsRefused() {
        val framework =
            AbaReference(
                listOf("a0", "a1"),
                listOf("x" to listOf("a0", "a1")),
                mapOf("a0" to "x", "a1" to "y"),
                setOf("a1" to "a0"),
            )
        assertFalse(framework.weakContraposition)
        assertEquals(setOf(setOf("a0"), setOf("a1")), framework.preferred)
        assertTrue(refused(framework.theory("aba_plus", "preferred")))
        // a0 deriving y contraposes it
        val contraposed =
            AbaReference(framework.assumptions, framework.rules + ("y" to listOf("a0")), framework.contraries, framework.weaker)
        assertTrue(contraposed.weakContraposition)
        assertEquals(contraposed.preferred, labelled(contraposed.theory("aba_plus", "preferred")))
    }

    // the labellings of arg2p, for every semantics, those of the definitions
    private fun agree(
        framework: AbaReference,
        profile: String,
    ) {
        framework.grounded?.let { assertEquals(setOf(it), labelled(framework.theory(profile)), framework.theory(profile)) }
        mapOf("preferred" to framework.preferred, "stable" to framework.stable, "complete" to framework.completes)
            .filterValues { it.isNotEmpty() }
            .forEach { (semantics, expected) ->
                assertEquals(
                    expected,
                    labelled(framework.theory(profile, semantics)),
                    "$semantics\n" + framework.theory(profile, semantics),
                )
            }
    }

    @Test
    fun flatAbaAgreesWithTheDefinitions() {
        val random = Random(1997)
        repeat(40) { agree(AbaReference.random(random, flat = true, preferences = false), "aba") }
    }

    @Test
    fun abaPlusAgreesWithTheDefinitionsUnderWeakContraposition() {
        val random = Random(2016)
        var compared = 0
        var refusals = 0
        repeat(600) {
            val framework = AbaReference.random(random, flat = true, preferences = true)
            when {
                !framework.reversing -> Unit
                framework.weakContraposition ->
                    if (compared < 15) {
                        agree(framework, "aba_plus")
                        compared++
                    }
                refusals < 8 -> {
                    assertTrue(refused(framework.theory("aba_plus")), framework.theory("aba_plus"))
                    refusals++
                }
            }
        }
        assertEquals(15, compared)
        assertEquals(8, refusals)
    }

    @Test
    fun aNonFlatTheoryIsRefused() {
        val random = Random(2009)
        var nonFlat = 0
        repeat(20) {
            val framework = AbaReference.random(random, flat = false, preferences = false)
            if (framework.rules.any { it.first in framework.assumptions }) {
                nonFlat++
                assertTrue(refused(framework.theory("aba")), framework.theory("aba"))
            }
        }
        assertTrue(nonFlat >= 5, "$nonFlat theories not flat")
    }
}
