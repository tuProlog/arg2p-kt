package it.unibo.tuprolog.argumentation.core.modules.data

import it.unibo.tuprolog.argumentation.core.model.terms.RuleTerm
import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The terms of the rules and premises of a theory as the parser converts them (module `rule`): a clause
 * `rl(Conclusion) :- [Id, Premises, Conclusion]` for a rule, Premises a conjunction, and
 * `rl(Conclusion) :- [Id, Conclusion]` for a premise, kept in the context by their conclusion. Kotlin builds them
 * through [RuleTerm.rule] and [RuleTerm.premise].
 */
object RuleModule : PrologModule("prolog.argumentation.data.rule", "rule") {
    override val prologRawTheory: String =
        // language=Prolog
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
}
