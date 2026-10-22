package it.unibo.tuprolog.argumentation.core.libs.data

import it.unibo.tuprolog.argumentation.core.libs.PrologModule
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term

/**
 * The term of an attack (module `attack`): `attack(Type, Attacker, Attacked, On)`, Attacker attacking Attacked on its
 * sub-argument On (Attacked itself for a direct attack), with an attack of Type (rebut, undermine, undercut, ...).
 * Kotlin reads and builds it through [parts] and [of].
 */
object AttackTerm : PrologModule("prolog.argumentation.data.attack", "attack") {
    override val prologRawTheory: String =
        """
        %! new(?Type, ?Attacker, ?Attacked, ?On, ?Attack)
        %  Attack is made of its parts.
        new(Type, Attacker, Attacked, On, attack(Type, Attacker, Attacked, On)).

        %! type(?Attack, ?Type)
        type(attack(Type, _, _, _), Type).

        %! attacker(?Attack, ?Attacker)
        attacker(attack(_, Attacker, _, _), Attacker).

        %! attacked(?Attack, ?Attacked)
        attacked(attack(_, _, Attacked, _), Attacked).

        %! on(?Attack, ?On)
        %  On is the sub-argument of the attacked argument the attack is on.
        on(attack(_, _, _, On), On).
        """.trimIndent()

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
