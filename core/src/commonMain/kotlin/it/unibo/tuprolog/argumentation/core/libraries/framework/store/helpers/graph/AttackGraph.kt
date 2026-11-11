package it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.graph

import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.mining.attacks

private val NO_IDS = IntArray(0)

/** The attacks between the arguments of a branch, by argument identifier, with the attackers and the attacked of each */
internal class AttackGraph(
    attackerIds: IntArray,
    attackedIds: IntArray,
) {
    private val attackersOf = grouped(attackedIds, attackerIds)
    private val attackedOf = grouped(attackerIds, attackedIds)

    /** The attackers of the argument [id], in the order of the attacks (one for each attack) */
    fun attackers(id: Int): IntArray = attackersOf.getOrNull(id) ?: NO_IDS

    /** The arguments the argument [id] attacks, in the order of the attacks (one for each attack) */
    fun attacked(id: Int): IntArray = attackedOf.getOrNull(id) ?: NO_IDS

    /** Whether the argument [a] attacks the argument [b] */
    fun attacks(
        a: Int,
        b: Int,
    ): Boolean = b in attacked(a)

    /** Whether [a] attacks [b] or [b] attacks [a] */
    fun conflict(
        a: Int,
        b: Int,
    ): Boolean = attacks(a, b) || attacks(b, a)

    fun selfAttacking(a: Int): Boolean = attacks(a, a)

    private companion object {
        // For each key, its values in order
        fun grouped(
            keys: IntArray,
            values: IntArray,
        ): Array<IntArray?> {
            val bound = (keys.maxOrNull() ?: -1) + 1
            val counts = IntArray(bound)
            for (key in keys) counts[key]++
            val groups = arrayOfNulls<IntArray>(bound)
            for (i in keys.indices) {
                val key = keys[i]
                val group = groups[key] ?: IntArray(counts[key]).also { groups[key] = it }
                group[group.size - counts[key]] = values[i]
                counts[key]--
            }
            return groups
        }
    }
}
