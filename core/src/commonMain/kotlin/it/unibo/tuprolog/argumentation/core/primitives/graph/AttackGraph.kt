package it.unibo.tuprolog.argumentation.core.primitives.graph

import it.unibo.tuprolog.core.Term

/** The attacks between the arguments of a branch, by argument identifier, with the attackers and the attacked of each */
internal class AttackGraph(
    /** The attacks, as (attacker, attacked) pairs, newest first */
    val attacks: List<Pair<Term, Term>>,
) {
    val attackers: Map<Term, List<Term>> by lazy(LazyThreadSafetyMode.NONE) { attacks.groupBy({ it.second }, { it.first }) }
    val attacked: Map<Term, List<Term>> by lazy(LazyThreadSafetyMode.NONE) { attacks.groupBy({ it.first }, { it.second }) }
}
