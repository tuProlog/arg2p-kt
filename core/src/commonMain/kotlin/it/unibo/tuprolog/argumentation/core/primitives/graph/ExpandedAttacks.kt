package it.unibo.tuprolog.argumentation.core.primitives.graph

import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.runtime.context.contextLibrary
import it.unibo.tuprolog.core.Term

/** An attack by the identifiers of its arguments: of [type], of [attacker] on [attacked], on its sub-argument [on] */
internal data class AttackIds(
    val type: Term,
    val attacker: Int,
    val attacked: Int,
    val on: Int,
)

/**
 * The attacks of a context branch: those saved (attackId/4, see the module graphStore) and those they extend to, an
 * attack on an argument being an attack on every argument built on it (supported by it, directly or not, see
 * supportId/2), on the same sub-argument. Only the attacks found are saved; they are extended here, once until the
 * attacks or the supports change, rather than saved for every argument built on the attacked one.
 *
 * The attacks come in the order of the saved ones, newest first, each preceded by those it extends to, as they would
 * have been saved; each attack comes once.
 */
internal class ExpandedAttacks(
    saved: List<AttackIds>,
    private val supported: Map<Int, List<Int>>,
) {
    private val above = HashMap<Int, List<Int>>()

    // The arguments built on an argument, directly or not, each once
    private fun superArguments(argument: Int): List<Int> =
        above.getOrPut(argument) {
            val found = LinkedHashSet<Int>()
            val pending = ArrayDeque(supported[argument].orEmpty())
            while (pending.isNotEmpty()) {
                val next = pending.removeFirst()
                if (found.add(next)) pending.addAll(supported[next].orEmpty())
            }
            found.toList()
        }

    val attacks: List<AttackIds> =
        LinkedHashSet<AttackIds>()
            .also { all ->
                saved.forEach { attack ->
                    superArguments(attack.attacked).asReversed().forEach { all += attack.copy(attacked = it) }
                    all += attack
                }
            }.toList()

    val byAttacker: Map<Int, List<AttackIds>> by lazy(LazyThreadSafetyMode.NONE) { attacks.groupBy { it.attacker } }

    val byAttacked: Map<Int, List<AttackIds>> by lazy(LazyThreadSafetyMode.NONE) { attacks.groupBy { it.attacked } }

    /** The attacks as (attacker, attacked) pairs */
    val graph: AttackGraph by lazy(LazyThreadSafetyMode.NONE) {
        AttackGraph(IntArray(attacks.size) { attacks[it].attacker }, IntArray(attacks.size) { attacks[it].attacked })
    }
}

// The expansion of the saved attacks of a branch, kept with them and redone when the supports change
private class Expansion(
    val saved: List<AttackIds>,
) {
    var supported: Map<Int, List<Int>>? = null
    var attacks: ExpandedAttacks? = null
}

// No support at all: the same map every time, so that the expansion is kept
private val NO_SUPPORTS: Map<Int, List<Int>> = emptyMap()

/** The attacks of the given branch (the active one if null), the saved ones and those they extend to */
internal fun NativeCall.expandedAttacks(branch: Int? = null): ExpandedAttacks {
    val supported =
        contextLibrary.derived("supportId/2", "supported", branch) { stored ->
            stored.map { it.clause.head!! }.groupBy({ it[0].id() }, { it[1].id() }).ifEmpty { NO_SUPPORTS }
        }
    val expansion =
        contextLibrary.derived("attackId/4", "expansion", branch) { stored ->
            Expansion(stored.map { it.clause.head!!.let { head -> AttackIds(head[0], head[1].id(), head[2].id(), head[3].id()) } })
        }
    if (expansion.supported !== supported) {
        expansion.attacks = ExpandedAttacks(expansion.saved, supported)
        expansion.supported = supported
    }
    return expansion.attacks!!
}

private fun Term.id(): Int = castToInteger().intValue.toInt()
