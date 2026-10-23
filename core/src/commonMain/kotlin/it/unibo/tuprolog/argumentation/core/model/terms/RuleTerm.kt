package it.unibo.tuprolog.argumentation.core.model.terms

import it.unibo.tuprolog.argumentation.core.modules.data.RuleModule
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term

/** The term of the module RuleModule as Kotlin reads and builds it (see [RuleModule]) */
object RuleTerm {
    /** The clause of the rule [id], concluding [conclusion] from [premises] (a conjunction) */
    fun rule(
        id: Term,
        premises: Term,
        conclusion: Term,
    ): Clause = Clause.of(Struct.of(HEAD, conclusion), List.of(id, premises, conclusion))

    /** The clause of the premise [id], concluding [conclusion] */
    fun premise(
        id: Term,
        conclusion: Term,
    ): Clause = Clause.of(Struct.of(HEAD, conclusion), List.of(id, conclusion))

    /** The clause of a rule or premise from the list in its body, [Id, Premises, Conclusion] or [Id, Conclusion] */
    fun of(parts: List): Clause? {
        val items = parts.toList()
        return when (items.size) {
            2, 3 -> Clause.of(Struct.of(HEAD, items.last()), parts)
            else -> null
        }
    }

    private const val HEAD = "rl"
}
