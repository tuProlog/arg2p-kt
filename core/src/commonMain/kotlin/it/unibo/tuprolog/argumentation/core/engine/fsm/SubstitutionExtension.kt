package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.runtime.isKnownGround
import it.unibo.tuprolog.argumentation.core.runtime.rebuilt
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
    return Substitution.unifier(Extended(this, bindings))
}

/**
 * The bindings of [substitution] extended with [bindings] (see [extendedWith]), as the map the unifier is built from:
 * the values of the substitution with the bindings applied, then the bindings, read as the unifier copies them (it
 * copies any map it is given: building one to be copied was a tenth of the allocations)
 */
private class Extended(
    private val substitution: Substitution.Unifier,
    private val bindings: Substitution.Unifier,
) : AbstractMap<Var, Term>() {
    override val size: Int
        get() = substitution.size + bindings.size

    override fun isEmpty(): Boolean = false

    override fun containsKey(key: Var): Boolean = bindings.containsKey(key) || substitution.containsKey(key)

    override fun get(key: Var): Term? = bindings[key] ?: substitution[key]?.substituted(bindings)

    override val entries: Set<Map.Entry<Var, Term>>
        get() = Entries()

    private inner class Entries : AbstractSet<Map.Entry<Var, Term>>() {
        override val size: Int
            get() = this@Extended.size

        override fun iterator(): Iterator<Map.Entry<Var, Term>> =
            object : Iterator<Map.Entry<Var, Term>> {
                private val extended = substitution.entries.iterator()
                private val added = bindings.entries.iterator()

                override fun hasNext(): Boolean = extended.hasNext() || added.hasNext()

                override fun next(): Map.Entry<Var, Term> =
                    if (extended.hasNext()) {
                        val (variable, value) = extended.next()
                        Binding(variable, value.substituted(bindings))
                    } else {
                        added.next()
                    }
            }
    }

    private class Binding(
        override val key: Var,
        override val value: Term,
    ) : Map.Entry<Var, Term>
}

/**
 * This term with [unifier] applied, in a single pass (arg2p): the parts ground at hand are kept (see [isKnownGround]),
 * the others rebuilt where a variable is bound. `Term.apply` collects the variables of every subterm (lazy sequences,
 * allocated at each level) to check them against the unifier before copying it. [unifier] binds the variables of its
 * own values to nothing (it is applied to itself, as the unifiers of the resolution are).
 */
internal fun Term.substituted(unifier: Substitution.Unifier): Term =
    when {
        unifier.isEmpty() -> this
        this is Var -> unifier[this] ?: this
        this is Struct && !isKnownGround() -> rebuilt { it.substituted(unifier) }
        else -> this
    }
