package it.unibo.tuprolog.argumentation.core.model.terms

import it.unibo.tuprolog.argumentation.core.modules.data.AttackModule
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term

/** The term of the module AttackModule as Kotlin reads and builds it (see [AttackModule]) */
object AttackTerm {
    /** The parts of the term of an attack, by name */
    class Parts(
        val type: Term,
        val attacker: Term,
        val attacked: Term,
        val on: Term,
    )

    /** The parts of [attack], the term of an attack; null if it is not one */
    fun parts(attack: Term): Parts? =
        (attack as? Struct)
            ?.takeIf { it.functor == FUNCTOR && it.arity == 4 }
            ?.let { Parts(it[0], it[1], it[2], it[3]) }

    /** The term of an attack, made of its parts */
    fun of(
        type: Term,
        attacker: Term,
        attacked: Term,
        on: Term,
    ): Struct = Struct.of(FUNCTOR, type, attacker, attacked, on)

    private const val FUNCTOR = "attack"
}
