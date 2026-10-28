package it.unibo.tuprolog.argumentation.core.modules.graph.extension

import kotlin.random.Random

/**
 * An ABA+ framework (Čyras and Toni 2016; ABA when there is no preference), flat or not, with its semantics computed
 * by brute force on the sets of assumptions, as defined: the rules (head, body), the contrary of each assumption, and
 * the strict preferences (weaker, stronger), closed transitively. For the tests of the profiles aba and aba_plus,
 * whose labels are those of the arguments.
 */
class AbaReference(
    val assumptions: List<String>,
    val rules: List<Pair<String, List<String>>>,
    val contraries: Map<String, String>,
    val weaker: Set<Pair<String, String>> = emptySet(),
) {
    /** The statements [set] derives, itself included */
    fun derived(set: Set<String>): Set<String> {
        val known = set.toMutableSet()
        do {
            val more = rules.filter { (head, body) -> head !in known && known.containsAll(body) }.map { it.first }
            known += more
        } while (more.isNotEmpty())
        return known
    }

    private fun closed(set: Set<String>) = derived(set).filter { it in assumptions }.toSet() == set

    private val subsets: List<Set<String>> =
        (0 until (1 shl assumptions.size)).map { mask -> assumptions.filterIndexed { i, _ -> mask and (1 shl i) != 0 }.toSet() }

    private fun subsetsOf(set: Set<String>) = subsets.filter { set.containsAll(it) }

    // A <-attacks B: normally, or reversed
    private fun attacks(
        a: Set<String>,
        b: Set<String>,
    ): Boolean =
        subsetsOf(a).any { part -> b.any { beta -> contraries.getValue(beta) in derived(part) && part.none { (it to beta) in weaker } } } ||
            subsetsOf(b).any { part ->
                a.any { alpha -> contraries.getValue(alpha) in derived(part) && part.any { (it to alpha) in weaker } }
            }

    // E defends [target] from every closed set attacking it
    private fun defends(
        e: Set<String>,
        target: Set<String>,
    ) = subsets.filter { closed(it) && attacks(it, target) }.all { attacks(e, it) }

    private val admissible: List<Set<String>> = subsets.filter { closed(it) && !attacks(it, it) && defends(it, it) }

    private val complete: List<Set<String>> =
        admissible.filter { e -> subsets.filter { defends(e, it) }.all { e.containsAll(it) } }

    /** The statements derived by the least complete extension, null if there is none */
    val grounded: Set<String>? = complete.firstOrNull { e -> complete.all { it.containsAll(e) } }?.let { derived(it) }

    /** The statements derived by each preferred extension (the maximal admissible ones) */
    val preferred: Set<Set<String>> =
        admissible.filter { e -> admissible.none { it != e && it.containsAll(e) } }.map { derived(it) }.toSet()

    /** The statements derived by each complete extension */
    val completes: Set<Set<String>> = complete.map { derived(it) }.toSet()

    /** The statements derived by each stable extension: closed, conflict-free, attacking each assumption left out */
    val stable: Set<Set<String>> =
        subsets
            .filter { e -> closed(e) && !attacks(e, e) && (assumptions - e).all { attacks(e, setOf(it)) } }
            .map { derived(it) }
            .toSet()

    // The supports of the deductions of each statement: the exact sets of assumptions they rest on
    private val supports: Map<String, Set<Set<String>>> by lazy {
        val statements = (assumptions + rules.flatMap { listOf(it.first) + it.second } + contraries.values).toSet()
        val supports = statements.associateWith { if (it in assumptions) mutableSetOf(setOf(it)) else mutableSetOf() }
        do {
            var changed = false
            rules.forEach { (head, body) ->
                body
                    .fold(listOf(emptySet<String>())) { acc, b -> acc.flatMap { partial -> supports.getValue(b).map { partial + it } } }
                    .forEach { if (supports.getValue(head).add(it)) changed = true }
            }
        } while (changed)
        supports
    }

    // The deductions of the contrary of an assumption from some weaker than it
    private val weakAttacks: List<Pair<Set<String>, String>> by lazy {
        assumptions.flatMap { beta ->
            supports[contraries.getValue(beta)].orEmpty().filter { a -> a.any { (it to beta) in weaker } }.map { it to beta }
        }
    }

    /** Some assumptions attack a stronger one: the preferences matter */
    val reversing: Boolean get() = weakAttacks.isNotEmpty()

    /**
     * Weak Contraposition (Čyras and Toni 2016, Axiom 4): if A derives the contrary of beta, some assumptions of A
     * weaker than it, then A but alpha, and beta, derive the contrary of alpha, for alpha one of the weakest of them
     */
    val weakContraposition: Boolean by lazy {
        weakAttacks.all { (a, beta) ->
            val weakerOnes = a.filter { (it to beta) in weaker }
            weakerOnes
                .filter { alpha -> weakerOnes.none { (it to alpha) in weaker } }
                .any { alpha -> supports[contraries.getValue(alpha)].orEmpty().any { d -> (a - alpha + beta).containsAll(d) } }
        }
    }

    /** The theory of arg2p for the framework, in [profile] (aba or aba_plus) */
    fun theory(
        profile: String,
        semantics: String = "grounded",
    ): String =
        buildString {
            appendLine("graphBuildMode($profile).")
            appendLine("argumentLabellingMode($semantics).")
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

    companion object {
        /**
         * A random framework where reversals matter: rules mostly from assumptions, several from the same ones,
         * concluding assumptions as well if not [flat]; preferences if [preferences]
         */
        fun random(
            random: Random,
            flat: Boolean,
            preferences: Boolean,
        ): AbaReference {
            val assumptions = List(random.nextInt(3, 6)) { "a$it" }
            val atoms = List(random.nextInt(2, 5)) { "p$it" }
            val sentences = assumptions + atoms
            val bodies = List(random.nextInt(1, 4)) { List(random.nextInt(1, 3)) { assumptions.random(random) }.distinct() }
            val rules =
                List(random.nextInt(2, 8)) {
                    val body = if (random.nextInt(4) == 0) listOf(sentences.random(random)) else bodies.random(random)
                    val head = if (!flat && random.nextInt(3) == 0) assumptions.random(random) else atoms.random(random)
                    head to body.filter { it != head }
                }
            val contraries = assumptions.associateWith { if (random.nextInt(4) == 0) sentences.random(random) else atoms.random(random) }
            val rank = assumptions.associateWith { random.nextInt(3) }
            val weaker =
                if (!preferences) {
                    emptySet()
                } else {
                    closure(
                        assumptions
                            .flatMap { w -> assumptions.map { s -> w to s } }
                            .filter { (w, s) -> rank.getValue(w) < rank.getValue(s) && random.nextInt(3) > 0 }
                            .toSet(),
                    )
                }
            return AbaReference(assumptions, rules, contraries, weaker)
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
}
