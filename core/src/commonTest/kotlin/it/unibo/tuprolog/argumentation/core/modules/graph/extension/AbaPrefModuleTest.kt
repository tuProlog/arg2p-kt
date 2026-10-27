package it.unibo.tuprolog.argumentation.core.modules.graph.extension

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.random.Random
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/**
 * ABA+ on the arguments of arg2p (the profile aba_plus, the extension abaPref) against the definitions of Čyras and
 * Toni (2016) on sets of assumptions, computed by brute force: the statements of the grounded labelling are those the
 * well-founded set of assumptions (the intersection of the complete ones) derives.
 */
class AbaPrefModuleTest {
    // A flat ABA+ framework: the rules (head, body), the contrary of each assumption, and the strict preferences
    // (weaker, stronger), closed transitively
    private class Framework(
        val assumptions: List<String>,
        val rules: List<Pair<String, List<String>>>,
        val contraries: Map<String, String>,
        val weaker: Set<Pair<String, String>>,
    ) {
        private fun derived(set: Set<String>): Set<String> {
            val known = set.toMutableSet()
            do {
                val more = rules.filter { (head, body) -> head !in known && known.containsAll(body) }.map { it.first }
                known += more
            } while (more.isNotEmpty())
            return known
        }

        private val subsets: List<Set<String>> =
            (0 until (1 shl assumptions.size)).map { mask -> assumptions.filterIndexed { i, _ -> mask and (1 shl i) != 0 }.toSet() }

        private fun subsetsOf(set: Set<String>) = subsets.filter { set.containsAll(it) }

        // A <-attacks B: normally, or reversed
        fun attacks(
            a: Set<String>,
            b: Set<String>,
        ): Boolean =
            subsetsOf(a).any { part ->
                b.any { beta -> contraries.getValue(beta) in derived(part) && part.none { (it to beta) in weaker } }
            } ||
                subsetsOf(b).any { part ->
                    a.any { alpha ->
                        contraries.getValue(alpha) in derived(part) &&
                            part.any { (it to alpha) in weaker }
                    }
                }

        private fun defends(
            e: Set<String>,
            target: Set<String>,
        ) = subsets.filter { attacks(it, target) }.all { attacks(e, it) }

        private val complete: List<Set<String>> =
            subsets.filter { e ->
                !attacks(e, e) && defends(e, e) && subsets.filter { defends(e, it) }.all { e.containsAll(it) }
            }

        /**
         * The statements the well-founded set derives; null if there is no complete extension (as may be without Weak
         * Contraposition, Čyras and Toni 2016)
         */
        val accepted: Set<String>? = complete.takeIf { it.isNotEmpty() }?.let { derived(it.reduce { all, e -> all intersect e }) }

        fun theory(): String =
            buildString {
                appendLine("graphBuildMode(aba_plus).")
                assumptions.forEach { appendLine("assumption($it).") }
                contraries.forEach { (a, c) -> appendLine("contrary($a, $c).") }
                rules.forEachIndexed {
                    i,
                    (head, body),
                    ->
                    appendLine("r$i : ${if (body.isEmpty()) "[]" else body.joinToString(", ")} -> $head.")
                }
                weaker.forEach { (w, s) -> appendLine("sup($s, $w).") }
            }
    }

    // The statements labelled in by the grounded labelling of arg2p
    private fun accepted(theory: String): Set<String> {
        val solver = Arg2pSolverFactory.default(theory = theory, settings = FlagsBuilder().create())
        val solution = solver.solve(Struct.parse("buildLabelSets(I, _, _)", solver.operators)).first()
        check(solution.isYes) { "$solution\n$theory" }
        return solution.substitution
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
    }

    // Example 13 of Čyras and Toni: {beta, beta2} attacks eps, though beta is weaker than it; the attack is reversed
    // on the set, so that eps also attacks the argument for the contrary of alpha resting on the same set
    @Test
    fun anAttackIsReversedOnEveryArgumentOfTheSameAssumptions() {
        val framework =
            Framework(
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
        assertEquals(setOf("alpha", "eps"), framework.accepted)
        assertEquals(framework.accepted, accepted(framework.theory()))
    }

    // Frameworks where reversals matter: rules mostly from assumptions, several from the same ones (arguments resting on
    // the same assumptions for different contraries), the contraries mostly symbols of their own
    @Test
    fun randomFrameworksAgreeWithTheDefinitions() {
        val random = Random(2016)
        var compared = 0
        repeat(60) {
            val assumptions = List(random.nextInt(3, 6)) { "a$it" }
            val atoms = List(random.nextInt(2, 5)) { "p$it" }
            val sentences = assumptions + atoms
            val bodies = List(random.nextInt(1, 4)) { List(random.nextInt(1, 3)) { assumptions.random(random) }.distinct() }
            val rules =
                List(random.nextInt(2, 8)) {
                    val body = if (random.nextInt(4) == 0) listOf(sentences.random(random)) else bodies.random(random)
                    atoms.random(random) to body
                }
            val contraries = assumptions.associateWith { if (random.nextInt(4) == 0) sentences.random(random) else atoms.random(random) }
            val rank = assumptions.associateWith { random.nextInt(3) }
            val weaker =
                assumptions
                    .flatMap { w -> assumptions.map { s -> w to s } }
                    .filter { (w, s) -> rank.getValue(w) < rank.getValue(s) && random.nextInt(3) > 0 }
                    .toSet()
                    .let { pairs -> closure(pairs) }
            val framework = Framework(assumptions, rules, contraries, weaker)
            framework.accepted?.let {
                compared++
                assertEquals(it, accepted(framework.theory()), framework.theory())
            }
        }
        assertTrue(compared >= 40, "$compared frameworks compared")
    }

    private fun closure(pairs: Set<Pair<String, String>>): Set<Pair<String, String>> {
        var closed = pairs
        do {
            val more = closed.flatMap { (a, b) -> closed.filter { it.first == b }.map { a to it.second } }.toSet() - closed
            closed = closed + more
        } while (more.isNotEmpty())
        return closed
    }
}
