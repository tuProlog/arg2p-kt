package it.unibo.tuprolog.argumentation.core.runtime.graph

import it.unibo.tuprolog.argumentation.core.runtime.context.Context
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextSchema
import it.unibo.tuprolog.argumentation.core.runtime.context.contextLibrary
import it.unibo.tuprolog.argumentation.engine.vm.ClauseStore
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
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
internal fun NativeCall.expandedAttacks(branch: Int? = null): ExpandedAttacks = contextLibrary.expandedAttacks(branch)

/** The arguments each argument supports directly, by their identifiers, in the given branch (the active one if null) */
internal fun Context.supportedBy(branch: Int? = null): Map<Int, List<Int>> =
    derived(ContextSchema.SUPPORT_PREDICATE, "supported", branch) { stored ->
        stored.groupBy({ it.idAt(0) }, { it.idAt(1) }).ifEmpty { NO_SUPPORTS }
    }

/** The supports of the given branch (the active one if null), newest first: the supporter, then the supported, of each */
internal fun Context.supportPairs(branch: Int? = null): IntArray =
    derived(ContextSchema.SUPPORT_PREDICATE, "supportPairs", branch) { stored ->
        IntArray(stored.size * 2) { i -> stored[i / 2].idAt(i % 2) }
    }

/** The attacks of the given branch (the active one if null), the saved ones and those they extend to */
internal fun Context.expandedAttacks(branch: Int? = null): ExpandedAttacks {
    val supported = supportedBy(branch)
    val expansion =
        derived(ContextSchema.ATTACK_PREDICATE, "expansion", branch) { stored ->
            Expansion(stored.map { AttackIds(it.typeAt(0), it.idAt(1), it.idAt(2), it.idAt(3)) })
        }
    if (expansion.supported !== supported) {
        expansion.attacks = ExpandedAttacks(expansion.saved, supported)
        expansion.supported = supported
    }
    return expansion.attacks!!
}

// The argument [i] of a stored fact, an identifier, read off its words
private fun ClauseStore.Stored.idAt(i: Int): Int {
    val id = record.idAt(i)
    return if (id >= 0) {
        id
    } else {
        record.clause.head!![i]
            .castToInteger()
            .intValue
            .toInt()
    }
}

// The argument [i] of a stored fact, a type of attack: the term the program keeps for an atom
private fun ClauseStore.Stored.typeAt(i: Int): Term = record.atomAt(i) ?: record.clause.head!![i]
