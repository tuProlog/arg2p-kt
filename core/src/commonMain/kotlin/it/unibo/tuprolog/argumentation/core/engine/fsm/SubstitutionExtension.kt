package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.core.Struct
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
    for ((variable, value) in this) extended[variable] = value.substituted(bindings)
    extended.putAll(bindings)
    return Substitution.unifier(extended)
}

/**
 * This term with [unifier] applied, in a single pass (arg2p): the ground parts are kept, the others rebuilt where a
 * variable is bound. `Term.apply` collects the variables of every subterm (lazy sequences, allocated at each level)
 * to check them against the unifier before copying it. [unifier] binds the variables of its own values to nothing
 * (it is applied to itself, as the unifiers of the resolution are).
 */
internal fun Term.substituted(unifier: Substitution.Unifier): Term =
    when {
        unifier.isEmpty() || isGround -> this
        this is Var -> unifier[this] ?: this
        this is Struct -> {
            var changed = false
            val arguments =
                List(arity) { i ->
                    val argument = this[i]
                    argument.substituted(unifier).also { if (it !== argument) changed = true }
                }
            if (changed) Struct.of(functor, arguments) else this
        }
        else -> this
    }
