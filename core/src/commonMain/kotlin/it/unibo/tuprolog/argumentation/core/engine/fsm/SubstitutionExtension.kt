package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var

/**
 * The substitution of a resolution extended with the bindings a step gives the variables of its goal (arg2p): the
 * goal had the substitution applied, so the bindings are of variables it does not bind and cannot contradict it, and
 * they are only applied to its terms. As `substitution + bindings`, without checking for contradictions and copying
 * the terms one time less.
 */
internal fun Substitution.Unifier.extendedWith(bindings: Substitution.Unifier): Substitution.Unifier {
    if (bindings.isEmpty()) return this
    if (isEmpty()) return bindings
    val extended = LinkedHashMap<Var, Term>(size + bindings.size)
    for ((variable, value) in this) extended[variable] = value.apply(bindings)
    extended.putAll(bindings)
    return Substitution.unifier(extended)
}
