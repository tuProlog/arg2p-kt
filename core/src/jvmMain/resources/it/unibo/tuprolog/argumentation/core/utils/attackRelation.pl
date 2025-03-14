%------------------------------------------------------------------------
% Attack definition
%------------------------------------------------------------------------

attacks(rebut, A, B) :- rebuts(A, B), !.
attacks(contrary_rebut, A, B) :- contraryRebuts(A, B), !.
attacks(undermine, A, B) :- undermines(A, B), !.
attacks(contrary_undermine, A, B) :- contraryUndermines(A, B), !.
attacks(undercut, A, B) :- undercuts(A, B), !.

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
	expanded_conflict(RuleHeadA, RuleHeadB).

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
	expanded_conflict(RuleHeadA, RuleHeadB).

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
undercuts([_, _, NegRuleB, _, _], [_, RuleB, _, _, [[RuleB], _, _]]) :-
    expanded_conflict(NegRuleB, [RuleB]).


%------------------------------------------------------------------------
% Utility to find candidate attackers on argument
%------------------------------------------------------------------------
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

% check(-Atom).

% conflict([Atom], [-Atom]) :- \+ check(Atom).
conflict([Atom], [-Atom], (Atom \= -_)).
conflict([-Atom], [Atom]).

% conflict([o(Atom)], [o(-Atom)]) :- \+ check(Atom).
conflict([o(Atom)], [o(-Atom)], (Atom \= -_)).
conflict([o(-Atom)], [o(Atom)]).

% conflict([o(Lit)], [-o(Lit)]).
% conflict([-o(Lit)], [o(Lit)]).

% conflict([p(Atom)], [o(-Atom)]) :- \+ check(Atom).
conflict([p(Atom)], [o(-Atom)], (Atom \= -_)).
conflict([o(-Atom)], [p(Atom)]).

conflict([p(-Atom)], [o(Atom)]).
% conflict([o(Atom)], [p(-Atom)]) :- \+ check(Atom).
conflict([o(Atom)], [p(-Atom)], (Atom \= -_)).

% BP CONFLICT

% conflict([bp(Atom)], [-bp(Atom)]).
% conflict([-bp(Atom)], [bp(Atom)]).

% SUP CONFLICT

conflict([sup(X, Y)], [sup(Y, X)]).
