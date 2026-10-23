package it.unibo.tuprolog.argumentation.core.modules.data

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The term of an attack (module `attack`): `attack(Type, Attacker, Attacked, On)`, Attacker attacking Attacked on its
 * sub-argument On (Attacked itself for a direct attack), with an attack of Type (rebut, undermine, undercut, ...).
 * Kotlin reads and builds it through [parts] and [of].
 */
object AttackModule : PrologModule("prolog.argumentation.data.attack", "attack") {
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
}
