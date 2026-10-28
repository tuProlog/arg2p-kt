package it.unibo.tuprolog.argumentation.core.primitives.graph

import it.unibo.tuprolog.argumentation.core.runtime.context.contextLibrary
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.Solve

/** An attack by the identifiers of its arguments: of [type], of [attacker] on [attacked], on its sub-argument [on] */
internal data class AttackIds(
    val type: Term,
    val attacker: Term,
    val attacked: Term,
    val on: Term,
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
    private val supported: Map<Term, List<Term>>,
) {
    private val above = mutableMapOf<Term, List<Term>>()

    // The arguments built on an argument, directly or not, each once
    private fun superArguments(argument: Term): List<Term> =
        above.getOrPut(argument) {
            val found = LinkedHashSet<Term>()
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

    val byAttacker: Map<Term, List<AttackIds>> by lazy(LazyThreadSafetyMode.NONE) { attacks.groupBy { it.attacker } }

    val byAttacked: Map<Term, List<AttackIds>> by lazy(LazyThreadSafetyMode.NONE) { attacks.groupBy { it.attacked } }

    /** The attacks as (attacker, attacked) pairs */
    val graph: AttackGraph by lazy(LazyThreadSafetyMode.NONE) { AttackGraph(attacks.map { it.attacker to it.attacked }) }

    /** The conflicts between the arguments */
    val conflicts: ConflictGraph<Term> by lazy(LazyThreadSafetyMode.NONE) { ConflictGraph(graph.attacks) }
}

// The expansion of the saved attacks of a branch, kept with them and redone when the supports change
private class Expansion(
    val saved: List<AttackIds>,
) {
    var supported: Map<Term, List<Term>>? = null
    var attacks: ExpandedAttacks? = null
}

// No support at all: the same map every time, so that the expansion is kept
private val NO_SUPPORTS: Map<Term, List<Term>> = emptyMap()

/** The attacks of the given branch (the active one if null), the saved ones and those they extend to */
internal fun Solve.Request<ExecutionContext>.expandedAttacks(branch: Int? = null): ExpandedAttacks {
    val supported =
        contextLibrary.derived("supportId", 2, "supported", branch) { clauses ->
            clauses.groupBy({ it.head!![0] }, { it.head!![1] }).ifEmpty { NO_SUPPORTS }
        }
    val expansion =
        contextLibrary.derived("attackId", 4, "expansion", branch) { clauses ->
            Expansion(clauses.map { it.head!!.let { head -> AttackIds(head[0], head[1], head[2], head[3]) } })
        }
    if (expansion.supported !== supported) {
        expansion.attacks = ExpandedAttacks(expansion.saved, supported)
        expansion.supported = supported
    }
    return expansion.attacks!!
}
