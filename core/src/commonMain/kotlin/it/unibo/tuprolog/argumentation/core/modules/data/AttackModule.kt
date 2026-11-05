package it.unibo.tuprolog.argumentation.core.modules.data

import it.unibo.tuprolog.argumentation.core.model.terms.AttackTerm
import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The term of an attack (module `attack`): `attack(Type, Attacker, Attacked, On)`, Attacker attacking Attacked on its
 * sub-argument On (Attacked itself for a direct attack), with an attack of Type (rebut, undermine, undercut, ...).
 * Kotlin reads and builds it through [AttackTerm.parts] and [AttackTerm.of].
 */
object AttackModule : PrologModule("prolog.argumentation.data.attack", "attack") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! new(?Type, ?Attacker, ?Attacked, ?On, ?Attack)
        %  Attack is made of its parts.
        new(Type, Attacker, Attacked, On, attack(Type, Attacker, Attacked, On)).
        """.trimIndent()
}
