package it.unibo.tuprolog.argumentation.core.primitives.graph

/** Conflicts (attacks in either direction) between nodes */
internal class ConflictGraph<T>(
    attacks: List<Pair<T, T>>,
) {
    private val attacked = attacks.groupBy({ it.first }, { it.second }).mapValues { it.value.toSet() }

    fun attacks(
        a: T,
        b: T,
    ) = attacked[a]?.contains(b) == true

    fun conflict(
        a: T,
        b: T,
    ) = attacks(a, b) || attacks(b, a)

    fun selfAttacking(a: T) = attacks(a, a)
}
