package it.unibo.tuprolog.argumentation.core.model

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse

interface ArgItem {
    val identifier: RuleIdentifier
    val conclusion: Term

    fun toTerm(): it.unibo.tuprolog.core.Term {
        return Struct.parse(this.toString())
    }
}

data class Premise(override val identifier: RuleIdentifier, override val conclusion: Term, val strict: Boolean = false) : ArgItem {
    override fun toString(): String {
        return "premise([$identifier, $conclusion])"
    }
}

data class Rule(override val identifier: RuleIdentifier, val premises: List<Term>, override val conclusion: Term, val strict: Boolean = false) : ArgItem {
    override fun toString(): String {
        return "rule([$identifier, $premises, $conclusion])"
    }
}
