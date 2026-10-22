%------------------------------------------------------------------------
% Attack definition
%------------------------------------------------------------------------

attacks(rebut, A, B) :- rebuts(A, B), !.
attacks(contrary_rebut, A, B) :- contraryRebuts(A, B), !.
attacks(undermine, A, B) :- undermines(A, B), !.
attacks(contrary_undermine, A, B) :- contraryUndermines(A, B), !.
attacks(undercut, A, B) :- undercuts(A, B), !.

% With metaConflicts the attacking arguments are the copies built on the arguments for conflicts (see
% standard_af::buildAdditionalArguments): each conflicts through the conflict of its argument only
expanded_conflict([mc(_, Conflict)|_], HeadA, HeadB) :- !,
    conflict_of(Conflict, HeadA, HeadB).
expanded_conflict(_, HeadA, HeadB) :-
    expanded_conflict(HeadA, HeadB).

conflict_of(conflict(A, B), [A], [B]).
conflict_of(conflict(A, B, Guard), [A], [B]) :-
    (callable(Guard) -> call(Guard); Guard).

expanded_conflict(HeadA, HeadB) :-
    parser::classic_conflict(HeadA, HeadB).
expanded_conflict(HeadA, HeadB) :-
    parser::classic_conflict(HeadA, HeadB, Guard),
    (callable(Guard) -> call(Guard); Guard).

%------------------------------------------------------------------------
% Rebutting definition: clash of incompatible conclusions
% we assume a preference relation over arguments determining whether two
% rebutting arguments mutually attack each other or only one of them
% (being preferred) attacks the other
%------------------------------------------------------------------------
rebuts([IDPremisesA, RuleA, RuleHeadA, _, _], [IDPremisesB, RuleB, RuleHeadB, _, Info]) :-
	RuleB \== none,
    Info \== [[], [], []],
	expanded_conflict(IDPremisesA, RuleHeadA, RuleHeadB).

%------------------------------------------------------------------------
% Contrary Rebutting definition: clash of a conclusion with a failure as premise assumption
%------------------------------------------------------------------------
contraryRebuts([IDPremisesA, RuleA, [RuleHeadA], _, _], [IDPremisesB, RuleB, RuleHeadB, Body, Info]) :-
	RuleA \== none,
	RuleB \== none,
	utils::contains(~(RuleHeadA), Body).

%------------------------------------------------------------------------
% Undermining definition: clash of incompatible premises
%------------------------------------------------------------------------
undermines([IDPremisesA, RuleA, RuleHeadA, _, _], [[IDPremiseB], none, RuleHeadB, _, Info]) :-
	Info \== [[], [], []],
	expanded_conflict(IDPremisesA, RuleHeadA, RuleHeadB).

%------------------------------------------------------------------------
% Contrary Undermining definition
%------------------------------------------------------------------------
contraryUndermines([IDPremisesA, none, [RuleHeadA], _, _], [IDPremisesB, RuleB, RuleHeadB, Body, Info]) :-
	RuleB \== none,
	utils::contains(~(RuleHeadA), Body).

%------------------------------------------------------------------------
% Undercutting definition: attacks on defeasible inference rule
%------------------------------------------------------------------------
undercuts([_, _, [undercut(RuleB)], _, _], [_, RuleB, _, _, [[RuleB], _, _]]).

%------------------------------------------------------------------------
% Raw Undercutting definition: attacks on defeasible inference rule
%------------------------------------------------------------------------
undercuts([IDPremisesA, _, NegRuleB, _, _], [_, RuleB, _, _, [[RuleB], _, _]]) :-
    expanded_conflict(IDPremisesA, NegRuleB, [RuleB]).


%------------------------------------------------------------------------
% Utility to find candidate attackers on argument
%------------------------------------------------------------------------

% possiblyAttacked(+Argument, -Statement): the statements whose arguments Argument may attack (the converse of
% findPossibleAttackers/2): those in conflict with its conclusion, the conclusions of the rules it undercuts and
% of the rules with its conclusion as a contrary premise
possiblyAttacked([_, _, Head, _, _], S) :-
    conflicting(Head, X),
    attackedStatement(X, S).
possiblyAttacked([_, _, [C], _, _], S) :-
    parser::classic_rule(_, Premises, [S]),
    member(~(C), Premises).

conflicting(Head, X) :- parser::classic_conflict(Head, [X], _).
conflicting(Head, X) :- parser::classic_conflict(Head, [X]).
conflicting([undercut(Rule)], Rule).

% A conflict is with a statement, or with a rule (an undercut)
attackedStatement(X, X).
attackedStatement(Rule, S) :- parser::classic_rule(Rule, _, [S]).

% attackDependency(+Argument, -Statement) and attackDependant(+Argument, -Statement): with metaConflicts the
% conflicts are themselves conclusions. Argument depends on the arguments that make others conflict with it; the
% arguments that make it conflict with others depend on it, and, if it is for a conflict, so do those it makes
% conflict
attackDependency([_, _, Head, _, _], S) :- parser::conflict_statement(_, Head, S).
attackDependency([_, Rule, _, _, _], S) :- Rule \== none, parser::conflict_statement(_, [Rule], S).
attackDependant([_, _, Head, _, _], S) :- parser::conflict_statement(Head, _, S).
attackDependant([_, _, [Conflict], _, _], S) :-
    parser::conflict_statement([A], [B], Conflict),
    (S = A ; S = B).

% possibleAttackers(+Id, +Argument, -Conflict): findPossibleAttackers/2 for the argument with identifier Id, looked
% for once: query mode looks for them to build the graph, and again to build the attacks
possibleAttackers(Id, _, Conflict) :-
    context_check(possibleAttackersOf(Id, Conflicts)), !,
    member(Conflict, Conflicts).
possibleAttackers(Id, Argument, Conflict) :-
    findall(C, findPossibleAttackers(Argument, C), Conflicts),
    context_assert(possibleAttackersOf(Id, Conflicts)),
    member(Conflict, Conflicts).

findPossibleAttackers([_, _, Head, _, _], Conf) :-
    parser::classic_conflict(Conf, Head, Guard).
findPossibleAttackers([_, _, Head, _, _], Conf) :-
    parser::classic_conflict(Conf, Head).
findPossibleAttackers([_, _, _, Prem, _], [Conf]) :-
    member(~(Conf), Prem).
findPossibleAttackers([_, RuleID, _, _, _], [undercut(RuleID)]).
findPossibleAttackers([_, RuleID, _, _, _], Conf) :-
    parser::classic_conflict(Conf, [RuleID], Guard).
findPossibleAttackers([_, RuleID, _, _, _], Conf) :-
    parser::classic_conflict(Conf, [RuleID]).



%========================================================================
% CONFLICT DEFINITION (NOT USED)
%========================================================================

%% check(-Atom).

%% conflict([Atom], [-Atom]) :- \+ check(Atom).
%conflict([Atom], [-Atom], (Atom \= -_)).
%conflict([-Atom], [Atom]).

%% conflict([o(Atom)], [o(-Atom)]) :- \+ check(Atom).
%conflict([o(Atom)], [o(-Atom)], (Atom \= -_)).
%conflict([o(-Atom)], [o(Atom)]).

%% conflict([o(Lit)], [-o(Lit)]).
%% conflict([-o(Lit)], [o(Lit)]).

%% conflict([p(Atom)], [o(-Atom)]) :- \+ check(Atom).
%conflict([p(Atom)], [o(-Atom)], (Atom \= -_)).
%conflict([o(-Atom)], [p(Atom)]).

%conflict([p(-Atom)], [o(Atom)]).
%% conflict([o(Atom)], [p(-Atom)]) :- \+ check(Atom).
%conflict([o(Atom)], [p(-Atom)], (Atom \= -_)).

%% BP CONFLICT

%% conflict([bp(Atom)], [-bp(Atom)]).
%% conflict([-bp(Atom)], [bp(Atom)]).

%% SUP CONFLICT

%conflict([sup(X, Y)], [sup(Y, X)]).
