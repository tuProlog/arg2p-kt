package it.unibo.tuprolog.argumentation.core.model

import it.unibo.tuprolog.argumentation.core.modules.data.RuleTerm
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.core.List as LogicList
import it.unibo.tuprolog.core.Term as LogicTerm

interface ArgItem {
    val identifier: RuleIdentifier
    val conclusion: Term

    /** The clause the parser converts the item to (see [RuleTerm]) */
    fun toTerm(): LogicTerm
}

data class Premise(
    override val identifier: RuleIdentifier,
    override val conclusion: Term,
    val strict: Boolean = false,
) : ArgItem {
    override fun toTerm(): LogicTerm = RuleTerm.premise(LogicTerm.parse(identifier), LogicTerm.parse(conclusion))

    override fun toString(): String = toTerm().toString()
}

data class Rule(
    override val identifier: RuleIdentifier,
    val premises: List<Term>,
    override val conclusion: Term,
    val strict: Boolean = false,
) : ArgItem {
    override fun toTerm(): LogicTerm =
        RuleTerm.rule(
            LogicTerm.parse(identifier),
            LogicList.of(premises.map { LogicTerm.parse(it) }),
            LogicTerm.parse(conclusion),
        )

    override fun toString(): String = toTerm().toString()
}
