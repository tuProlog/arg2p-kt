package it.unibo.tuprolog.argumentation.core.model

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse

data class Attack(val attacker: Argument, val target: Argument, val type: AttackType? = null, val on: Argument? = null) {
    override fun toString(): String {
        return "attack(${type ?: "none"}, ${attacker.termRepresentation()}, ${target.termRepresentation()}, ${on?.termRepresentation() ?: "none"})"
    }

    fun toTerm(): Term {
        return Struct.parse(this.toString())
    }
}
