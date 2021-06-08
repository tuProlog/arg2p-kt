modifyArgumentationGraph(preferences, Originals, [Arguments, Attacks, Supports], [NewArguments, NewAttacks, NewSupports]) :-
    checkPreferences(Originals, [Arguments, Attacks, Supports], [NewArguments, NewAttacks, NewSupports]), !.

checkPreferences([], [Arguments, Attacks, Supports], [Arguments, Attacks, Supports]).
checkPreferences([A|Attacks], [Arguments, Attacks, Supports], [NewArguments, NewAttacks, NewSupports]) :-
    checkPreferences(Attacks, [Arguments, Attacks, Supports], [TempArguments, TempAttacks, TempSupports]),
    checkPreference(A, [TempArguments, TempAttacks, TempSupports], [NewArguments, NewAttacks, NewSupports]).

checkPreference(A, [Arguments, Attacks, Supports], [[Conflict|NewArguments], [(prefrebut, Conflict, [Id, TopRule, Conc])|NewAttacks], NewSupports]) :-
    notDefeat(A),
    convertAttack(A, [Arguments, Attacks, Supports], [Id, TopRule, Conc], [NewArguments, NewAttacks, NewSupports]),
    Conflict = [[artPref|Id], artPref, [neg, defeat(A)]],
    asserta(argument(Conflict)),
    asserta(attack(prefrebut, Conflict, [Id, TopRule, Conc])),
    asserta(attack(prefrebut, Conflict, [Id, TopRule, Conc], [Id, TopRule, Conc])).
checkPreference(A, [Arguments, Attacks, Supports], [Arguments, Attacks, Supports]) :-
    \+ notDefeat(A).

notDefeat((rebut, A, B, C)) :- superiorArgument(B, A, C).
notDefeat((undermine, A, B, C)) :- superiorArgument(B, A, C).

%------------------------------------------------------------------------
% Superiority definition
% A superiority relation over a set of rules Rules is an antireflexive and
% antisymmetric binary relation over Rules
%------------------------------------------------------------------------

superiorArgument(_, B, C) :- orderingComparator(normal), superiorArgument(C, B).
superiorArgument(A, B, _) :- \+ orderingComparator(normal), superiorArgument(A, B).

superiorArgument(A, B) :-
	argumentInfo(A, [LastDefRulesA, DefRulesA, DefPremisesA]),
	argumentInfo(B, [LastDefRulesB, DefRulesB, DefPremisesB]),
	superiorArgument(LastDefRulesA, DefRulesA, DefPremisesA, LastDefRulesB, DefRulesB, DefPremisesB).

superiorArgument(LastDefRulesA, _, DefPremisesA, LastDefRulesB, _, DefPremisesB) :-
    orderingPrinciple(last),
	superior(LastDefRulesA, DefPremisesA, LastDefRulesB, DefPremisesB).

superiorArgument(_, DefRulesA, DefPremisesA, _, DefRulesB, DefPremisesB) :-
    orderingPrinciple(weakest),
	superior(DefRulesA, DefPremisesA, DefRulesB, DefPremisesB).

superior([], PremisesA, [], PremisesB) :-
	weaker(PremisesB, PremisesA).
superior(DefRulesA, _, DefRulesB, _) :-
	orderingPrinciple(last),
	(DefRulesA \== []; DefRulesB \== []),
	weaker(DefRulesB, DefRulesA).
superior(DefRulesA, [], DefRulesB, []) :-
	orderingPrinciple(weakest),
	weaker(DefRulesB, DefRulesA).
superior(DefRulesA, PremisesA, DefRulesB, PremisesB) :-
	orderingPrinciple(weakest),
	(DefRulesA \== []; DefRulesB \== []),
	(PremisesA \== []; PremisesB \== []),
	weaker(DefRulesB, DefRulesA),
	weaker(PremisesB, PremisesA).

weaker(RulesA, []) :-
	RulesA \== [].

weaker(RulesA, RulesB) :-
	RulesA \== [],
	RulesB \== [],
	orderingComparator(elitist),
	member(Rule, RulesA),
	allStronger(Rule, RulesB), !.

weaker(RulesA, RulesB) :-
	RulesA \== [],
	RulesB \== [],
	orderingComparator(democrat),
	weakerDemo(RulesA, RulesB).

%(A, B) ∈ attnr(K) iff 1. A undercuts B, or 2. A rebuts B (at B′)
% and there is no defeasible rule d ∈ ldr(A) such that d ≺ last(B′).
weaker(RulesA, RulesB) :-
	RulesA \== [],
	RulesB \== [],
	orderingComparator(normal),
	member(W, RulesA),
	member(X, RulesB),
	sup(X, W), !.

weakerDemo([], _).
weakerDemo([H|T], Rules) :-
	singleStronger(H, Rules),
	weakerDemo(T, Rules).

allStronger(_, []).
allStronger(Target, [Rule|Rules]) :-
	sup(Rule, Target),
	allStronger(Target, Rules).

singleStronger(Target, Rules) :-
	member(Rule, Rules),
	sup(Rule, Target), !.
