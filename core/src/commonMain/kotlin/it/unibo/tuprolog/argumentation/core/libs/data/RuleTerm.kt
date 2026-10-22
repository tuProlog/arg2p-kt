package it.unibo.tuprolog.argumentation.core.libs.data

import it.unibo.tuprolog.argumentation.core.libs.PrologModule
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term

/**
 * The terms of the rules and premises of a theory as the parser converts them (module `rule`): a clause
 * `rl(Conclusion) :- [Id, Premises, Conclusion]` for a rule, Premises a conjunction, and
 * `rl(Conclusion) :- [Id, Conclusion]` for a premise, kept in the context by their conclusion. Kotlin builds them
 * through [rule] and [premise].
 */
object RuleTerm : PrologModule("prolog.argumentation.data.rule", "rule") {
    override val prologRawTheory: String =
        """
        %! rule(?Id, ?Premises, ?Conclusion, ?Head, ?Body)
        %  Head :- Body is the clause of the rule Id, concluding Conclusion from Premises (a conjunction).
        rule(Id, Premises, Conclusion, rl(Conclusion), [Id, Premises, Conclusion]).

        %! premise(?Id, ?Conclusion, ?Head, ?Body)
        %  Head :- Body is the clause of the premise Id, concluding Conclusion.
        premise(Id, Conclusion, rl(Conclusion), [Id, Conclusion]).

        %! byConclusion(?Conclusion, ?Head)
        %  Head is the head of the clauses of the rules and premises concluding Conclusion.
        byConclusion(Conclusion, rl(Conclusion)).
        """.trimIndent()

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
