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
        %  build the graph, and again to build the attacks. When the conflicts are argued (see conflict::argued/0), only
        %  those that do not depend on the arguments for conflicts are kept: those that do are looked for each time, the
        %  arguments for conflicts being those saved by then.
        possibleAttackers(Id, Argument, Statement) :-
            context_check(possibleAttackersOf(Id, Statements)), !,
            keptOrArgued(Statements, Argument, Statement).
        possibleAttackers(Id, Argument, Statement) :-
            findall(S, kept(Argument, S), Statements),
            context_assert(possibleAttackersOf(Id, Statements)),
            keptOrArgued(Statements, Argument, Statement).

        %! attackersOf(+Id, +Argument, -Statement)
        %  As possibleAttackers/3, what is found not kept: for who looks once, as the builder of a whole graph.
        attackersOf(Id, Argument, Statement) :-
            context_check(possibleAttackersOf(Id, Statements)), !,
            keptOrArgued(Statements, Argument, Statement).
        attackersOf(_, Argument, Statement) :-
            findPossibleAttackers(Argument, Statement).

        % The possible attackers of Argument kept (Statements), and with the conflicts argued those found by the arguments
        % for conflicts saved now
        keptOrArgued(Statements, _, Statement) :-
            member(Statement, Statements).
        keptOrArgued(_, Argument, Statement) :-
            conflict::argued,
            arguedAttacker(Argument, Statement).

        % The possible attackers to keep: all of them, or with the conflicts argued those not found by arguments for conflicts
        kept(Argument, Statement) :-
            conflict::argued, !,
            staticAttacker(Argument, Statement).
        kept(Argument, Statement) :-
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

        % findPossibleAttackers/2 split by what the conflicts are: arguments (arguedAttacker/2, with the conflicts argued) or
        % not (staticAttacker/2)
        arguedAttacker(Argument, Statement) :-
            argument::conclusion(Argument, Conclusion),
            conflict::guarded(Statement, Conclusion, _).
        arguedAttacker(Argument, Statement) :-
            argument::conclusion(Argument, Conclusion),
            conflict::unguarded(Statement, Conclusion).
        arguedAttacker(Argument, Statement) :-
            argument::topRule(Argument, Rule),
            conflict::guarded(Statement, [Rule], _).
        arguedAttacker(Argument, Statement) :-
            argument::topRule(Argument, Rule),
            conflict::unguarded(Statement, [Rule]).

        staticAttacker(Argument, [Statement]) :-
            argument::body(Argument, Body),
            member(~(Statement), Body),
            attackRelation::static.
        staticAttacker(Argument, [undercut(Rule)]) :-
            argument::topRule(Argument, Rule),
            attackRelation::static.

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
            parser::ruleUsing(~(Claim), Statement),
            attackRelation::static.

        conflicting(Conclusion, Statement) :- conflict::guarded(Conclusion, [Statement], _).
        conflicting(Conclusion, Statement) :- conflict::unguarded(Conclusion, [Statement]).
        conflicting([undercut(Rule)], Rule) :- attackRelation::static.

        % A conflict is with a statement, or with a rule (an undercut)
        attackedStatement(Statement, Statement).
        attackedStatement(Rule, Statement) :- parser::ruleConclusion(Rule, Statement).
        """.trimIndent()
}
