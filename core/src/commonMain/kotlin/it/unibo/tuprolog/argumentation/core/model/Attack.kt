package it.unibo.tuprolog.argumentation.core.model

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse

data class Attack(
    val attacker: Argument,
    val
    target: Argument,
    val type: AttackType? = null,
    val on: Argument? = null,
) {
    override fun toString(): String {
        fun <T> parse(x: T?): Any = x ?: "none"

        return "attack(${parse(
            type,
        )}, ${attacker.termRepresentation()}, ${target.termRepresentation()}, ${parse(on?.termRepresentation())})"
    }

    fun toTerm(): Term = Struct.parse(this.toString())
}
