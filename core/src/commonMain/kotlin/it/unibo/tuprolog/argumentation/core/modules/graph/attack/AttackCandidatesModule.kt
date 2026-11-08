package it.unibo.tuprolog.argumentation.core.modules.graph.attack

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The statements whose arguments may attack an argument, or be attacked by it (module `attackCandidates`): the
 * graph builders only check the attacks (see the module attackRelation) between an argument and the arguments for these
 * statements, and query mode explores them.
 */
object AttackCandidatesModule : PrologModule("prolog.argumentation.attack.candidates", "attackCandidates") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! possibleAttackers(+Id, +Argument, -Statement)
        %  findPossibleAttackers/2 for the argument with identifier Id, looked for once: query mode looks for them to
        %  build the graph, and again to build the attacks. When the conflicts are argued (see conflict::argued/0) they are
        %  looked for each time, the arguments for conflicts being those saved by then.
        possibleAttackers(Id, _, Statement) :-
            context_check(possibleAttackersOf(Id, Statements)), !,
            member(Statement, Statements).
        possibleAttackers(_, Argument, Statement) :-
            conflict::argued, !,
            findPossibleAttackers(Argument, Statement).
        possibleAttackers(Id, Argument, Statement) :-
            findall(S, findPossibleAttackers(Argument, S), Statements),
            context_assert(possibleAttackersOf(Id, Statements)),
            member(Statement, Statements).

        %! attackersOf(+Id, +Argument, -Statement)
        %  As possibleAttackers/3 with the conflicts not argued, what is found not kept: for who looks once, as the builder
        %  of a whole graph.
        attackersOf(Id, _, Statement) :-
            context_check(possibleAttackersOf(Id, Statements)), !,
            member(Statement, Statements).
        attackersOf(_, Argument, Statement) :-
            findPossibleAttackers(Argument, Statement).

        %! forgetPossibleAttackers
        %  The possible attackers looked for forgotten: with arguments for conflicts built since (metaConflicts), they
        %  may be more.
        forgetPossibleAttackers :-
            context_retract(possibleAttackersOf(_, _)).

        %! findPossibleAttackers(+Argument, -Statement)
        %  The arguments for Statement may attack Argument: Statement conflicts with its conclusion or its top rule, it
        %  is the contrary of a premise of its top rule, or it undercuts its top rule.
        findPossibleAttackers(Argument, Statement) :-
            argument::conclusion(Argument, Conclusion),
            conflict::guarded(Statement, Conclusion, _).
        findPossibleAttackers(Argument, Statement) :-
            argument::conclusion(Argument, Conclusion),
            conflict::unguarded(Statement, Conclusion).
        findPossibleAttackers(Argument, [Statement]) :-
            argument::body(Argument, Body),
            member(~(Statement), Body),
            attackRelation::static.
        findPossibleAttackers(Argument, [undercut(Rule)]) :-
            argument::topRule(Argument, Rule),
            attackRelation::static.
        findPossibleAttackers(Argument, Statement) :-
            argument::topRule(Argument, Rule),
            conflict::guarded(Statement, [Rule], _).
        findPossibleAttackers(Argument, Statement) :-
            argument::topRule(Argument, Rule),
            conflict::unguarded(Statement, [Rule]).

        %! possiblyAttacked(+Argument, -Statement)
        %  Argument may attack the arguments for Statement (the converse of findPossibleAttackers/2): those in conflict
        %  with its conclusion, the conclusions of the rules it undercuts and of the rules with its conclusion as a
        %  contrary premise.
        possiblyAttacked(Argument, Statement) :-
            argument::conclusion(Argument, Conclusion),
            conflicting(Conclusion, Conflicting),
            attackedStatement(Conflicting, Statement).
        possiblyAttacked(Argument, Statement) :-
            argument::claim(Argument, Claim),
            theory::ruleUsing(~(Claim), Statement),
            attackRelation::static.

        conflicting(Conclusion, Statement) :- conflict::guarded(Conclusion, [Statement], _).
        conflicting(Conclusion, Statement) :- conflict::unguarded(Conclusion, [Statement]).
        conflicting([undercut(Rule)], Rule) :- attackRelation::static.

        % A conflict is with a statement, or with a rule (an undercut)
        attackedStatement(Statement, Statement).
        attackedStatement(Rule, Statement) :- theory::ruleConclusion(Rule, Statement).
        """.trimIndent()
}
