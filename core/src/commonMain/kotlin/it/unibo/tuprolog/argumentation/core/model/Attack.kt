package it.unibo.tuprolog.argumentation.core.model

data class Attack(val attacker: Argument, val target: Argument, val type: AttackType? = null, val on: Argument? = null) {
    override fun toString(): String {
        return "attack(${type ?: "none"}, $attacker, $target, ${on ?: "none"})"
    }
}
