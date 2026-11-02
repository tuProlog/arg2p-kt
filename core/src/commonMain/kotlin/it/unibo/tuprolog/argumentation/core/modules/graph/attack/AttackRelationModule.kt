package it.unibo.tuprolog.argumentation.core.modules.graph.attack

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * When an argument attacks another one directly (module `attackRelation`): rebutting or undermining it through a conflict
 * between their conclusions, rebutting or undermining it through a contrary premise, or undercutting its top rule.
 * Whether the attack succeeds is up to the graph extensions (preferences, restrictions).
 */
object AttackRelationModule : PrologModule("prolog.argumentation.attack", "attackRelation") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! attacks(?Type, +Attacker, +Attacked)
        %  Attacker attacks Attacked directly, Type being the first kind of attack that applies. With metaConflicts
        %  every attack goes through a conflict concluded by an argument (see the module conflictArguments): the
        %  contraries and the undercuts by undercut(Rule), which no such conflict states, do not apply (see static/0).
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
            utils::contains(~(Claim), Body),
            static.

        %! undermines(+A, +B)
        %  The conclusion of A conflicts with B, a defeasible premise (or its copy for a conflict, see the module
        %  conflictArguments).
        undermines(A, B) :-
            argument::isPremise(B),
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
            utils::contains(~(Claim), Body),
            static.

        %! undercuts(+A, +B)
        %  A concludes undercut(Rule), or a statement conflicting with [Rule], Rule being the defeasible top rule of B.
        undercuts(A, B) :-
            defeasibleTopRule(B, Rule),
            argument::claim(A, undercut(Rule)),
            static.
        undercuts(A, B) :-
            defeasibleTopRule(B, Rule),
            conflictsWith(A, [Rule]).

        %! static
        %  The attacks no conflict states apply (contraries, undercut(Rule)): unless the conflicts are concluded by
        %  arguments (metaConflicts, see conflict::argued/0, read here as the setting: a call to another module costs as
        %  much as the graph it is made over).
        static :- setting(metaConflicts), !, fail.
        static.

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
