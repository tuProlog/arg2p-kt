package it.unibo.tuprolog.argumentation.core.libs.graph.attack

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

/**
 * When an argument attacks another one directly (module `attack`): rebutting or undermining it through a conflict
 * between their conclusions, rebutting or undermining it through a contrary premise, or undercutting its top rule.
 * Whether the attack succeeds is up to the graph extensions (preferences, restrictions).
 */
object AttackRelation : PrologModule("prolog.argumentation.attack", "attack") {
    override val prologRawTheory: String =
        """
        %! attacks(?Type, +Attacker, +Attacked)
        %  Attacker attacks Attacked directly, Type being the first kind of attack that applies.
        attacks(rebut, A, B) :- rebuts(A, B), !.
        attacks(contrary_rebut, A, B) :- contraryRebuts(A, B), !.
        attacks(undermine, A, B) :- undermines(A, B), !.
        attacks(contrary_undermine, A, B) :- contraryUndermines(A, B), !.
        attacks(undercut, A, B) :- undercuts(A, B), !.

        %! rebuts(+A, +B)
        %  The conclusion of A conflicts with the one of B, a defeasible argument with a top rule.
        rebuts(A, B) :-
            \+ argument::isPremise(B),
            argument::isDefeasible(B),
            argument::conclusion(B, Conclusion),
            conflictsWith(A, Conclusion).

        %! contraryRebuts(+A, +B)
        %  A concludes the contrary of a premise of the top rule of B (~(Claim)), both having a top rule.
        contraryRebuts(A, B) :-
            \+ argument::isPremise(A),
            \+ argument::isPremise(B),
            argument::claim(A, Claim),
            argument::body(B, Body),
            utils::contains(~(Claim), Body).

        %! undermines(+A, +B)
        %  The conclusion of A conflicts with B, a defeasible premise.
        undermines(A, B) :-
            argument::isPremise(B),
            argument::rules(B, [_]),
            argument::isDefeasible(B),
            argument::conclusion(B, Conclusion),
            conflictsWith(A, Conclusion).

        %! contraryUndermines(+A, +B)
        %  A, a premise, is the contrary of a premise of the top rule of B (~(Claim)).
        contraryUndermines(A, B) :-
            argument::isPremise(A),
            \+ argument::isPremise(B),
            argument::claim(A, Claim),
            argument::body(B, Body),
            utils::contains(~(Claim), Body).

        %! undercuts(+A, +B)
        %  A concludes undercut(Rule), or a statement conflicting with [Rule], Rule being the defeasible top rule of B.
        undercuts(A, B) :-
            defeasibleTopRule(B, Rule),
            argument::claim(A, undercut(Rule)).
        undercuts(A, B) :-
            defeasibleTopRule(B, Rule),
            conflictsWith(A, [Rule]).

        % The top rule of the argument, if it is its last defeasible rule
        defeasibleTopRule(Argument, Rule) :-
            argument::topRule(Argument, Rule),
            argument::lastDefeasibleRules(Argument, [Rule]).

        %! conflictsWith(+Argument, ?Statement)
        %  The conclusion of Argument conflicts with Statement: only through its own conflict for the copy of an argument
        %  made for a conflict argument (see the module conflictArguments), through any conflict otherwise.
        conflictsWith(Argument, Statement) :-
            conflictArguments::conflictOf(Argument, Conflict), !,
            argument::conclusion(Argument, Conclusion),
            conflict::stated(Conflict, Conclusion, Statement).
        conflictsWith(Argument, Statement) :-
            argument::conclusion(Argument, Conclusion),
            conflict::between(Conclusion, Statement).
        """.trimIndent()
}
