superiorArgument(A, B) :- superiorArgumentSupportBuffered(A, B, _).
superiorArgument(A, B, SupSet) :- superiorArgumentSupportBuffered(A, B, SupSet).

% Superiority only depends on the defeasible rules and premises of the arguments,
% so results are buffered by those (much smaller and more often shared than whole arguments)
superiorArgumentSupportBuffered([_, _, _, _, InfoA], [_, _, _, _, InfoB], _) :-
    context_check(superior(InfoA, InfoB, _, false)), !,
    fail.
superiorArgumentSupportBuffered([_, _, _, _, InfoA], [_, _, _, _, InfoB], SupSet) :-
    context_check(superior(InfoA, InfoB, SupSet, true)), !.
superiorArgumentSupportBuffered([R, T, C, B, InfoA], [RB, TB, CB, BB, InfoB], SupSet) :-
    superiorArgumentSupport([R, T, C, B, InfoA], [RB, TB, CB, BB, InfoB], SupSet), !,
    context_assert(superior(InfoA, InfoB, SupSet, true)).
superiorArgumentSupportBuffered([_, _, _, _, InfoA], [_, _, _, _, InfoB], _) :-
    context_assert(superior(InfoA, InfoB, [], false)),
    fail.

superiorArgumentSupport([_, _, _, _, [LastDefRulesA, DefRulesA, DefPremisesA]], [_, _, _, _, [LastDefRulesB, DefRulesB, DefPremisesB]], SupSet) :-
	superiorArgumentSupport(LastDefRulesA, DefRulesA, DefPremisesA, LastDefRulesB, DefRulesB, DefPremisesB, SupSet).

superiorArgumentSupport(LastDefRulesA, _, DefPremisesA, LastDefRulesB, _, DefPremisesB, SupSet) :-
    orderingPrinciple(last),
	superior(LastDefRulesA, DefPremisesA, LastDefRulesB, DefPremisesB, SupSet).

superiorArgumentSupport(_, DefRulesA, DefPremisesA, _, DefRulesB, DefPremisesB, SupSet) :-
    orderingPrinciple(weakest),
	superior(DefRulesA, DefPremisesA, DefRulesB, DefPremisesB, SupSet).

superior([], PremisesA, [], PremisesB, SupSet) :-
	weaker(PremisesB, PremisesA, SupSet).
superior(DefRulesA, _, DefRulesB, _, SupSet) :-
	orderingPrinciple(last),
	(DefRulesA \== []; DefRulesB \== []),
	weaker(DefRulesB, DefRulesA, SupSet).
superior(DefRulesA, [], DefRulesB, [], SupSet) :-
	orderingPrinciple(weakest),
	weaker(DefRulesB, DefRulesA, SupSet).
superior(DefRulesA, PremisesA, DefRulesB, PremisesB, SupSet) :-
	orderingPrinciple(weakest),
	(DefRulesA \== []; DefRulesB \== []),
	(PremisesA \== []; PremisesB \== []),
	weaker(DefRulesB, DefRulesA, SupSetA),
	weaker(PremisesB, PremisesA, SupSetB),
	utils::appendLists([SupSetA, SupSetB], SupSet).

weaker(RulesA, [], []) :-
	RulesA \== [].

weaker(RulesA, RulesB, SupSet) :-
	RulesA \== [],
	RulesB \== [],
	orderingComparator(elitist),
	member(Rule, RulesA),
	allStronger(Rule, RulesB, SupSet), !.

weaker(RulesA, RulesB, SupSet) :-
	RulesA \== [],
	RulesB \== [],
	orderingComparator(democrat),
	weakerDemo(RulesA, RulesB, SupSet).

%(A, B) ∈ attnr(K) iff 1. A undercuts B, or 2. A rebuts B (at B′)
% and there is no defeasible rule d ∈ ldr(A) such that d ≺ last(B′).

weaker(RulesA, RulesB, [sup(X, W)]) :-
	RulesA \== [],
	RulesB \== [],
	orderingComparator(normal),
	member(W, RulesA),
	member(X, RulesB),
	context_check(sup(X, W)), !.

weakerDemo([], _, []).
weakerDemo([H|T], Rules, [Sup|SupSet]) :-
	singleStronger(H, Rules, Sup),
	weakerDemo(T, Rules, SupSet).

singleStronger(Target, Rules, sup(Rule, Target)) :-
	member(Rule, Rules),
	context_check(sup(Rule, Target)), !.

allStronger(_, [], []).
allStronger(Target, [Rule|Rules], [sup(Rule, Target)|SupSet]) :-
	context_check(sup(Rule, Target)),
	allStronger(Target, Rules, SupSet).
