package it.unibo.tuprolog.argumentation.core.model

import it.unibo.tuprolog.argumentation.core.libs.data.AttackTerm
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse

data class Attack(
    val attacker: Argument,
    val target: Argument,
    val type: AttackType? = null,
    val on: Argument? = null,
) {
    override fun toString(): String = toTerm().toString()

    /** The term of the attack (see [AttackTerm]), none for the type or the sub-argument not known */
    fun toTerm(): Struct =
        AttackTerm.of(
            type?.let { Term.parse(it) } ?: Atom.of("none"),
            attacker.termRepresentation(),
            target.termRepresentation(),
            on?.termRepresentation() ?: Atom.of("none"),
        )
}
