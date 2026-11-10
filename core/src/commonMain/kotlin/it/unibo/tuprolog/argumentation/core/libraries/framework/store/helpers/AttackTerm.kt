package it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term

/** The term of an attack: `attack(Type, Attacker, Attacked, On)`, Attacker attacking Attacked on its sub-argument On */
object AttackTerm {
    /** The term of an attack, made of its parts */
    fun of(
        type: Term,
        attacker: Term,
        attacked: Term,
        on: Term,
    ): Struct = Struct.of(FUNCTOR, type, attacker, attacked, on)

    private const val FUNCTOR = "attack"
}
