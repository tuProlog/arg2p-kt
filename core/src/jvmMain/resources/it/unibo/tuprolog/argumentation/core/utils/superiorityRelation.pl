retractPreferenceCache :-
    cache_retract(preferences(_)),
    cache_retract(superior(_, _, _, _)).

setupPreferences(Pref) :-
    cache_assert(preferences(Pref)).

superiorArgument(A, B) :- superiorArgumentSupportBuffered(A, B, _).
superiorArgument(A, B, SupSet) :- superiorArgumentSupportBuffered(A, B, SupSet).

superiorArgumentSupportBuffered(A, B, SupSet) :-
    cache_check(superior(A, B, SupSet, true)), !.
superiorArgumentSupportBuffered(A, B, SupSet) :-
    cache_check(superior(A, B, SupSet, false)),
    fail, !.
superiorArgumentSupportBuffered(A, B, SupSet) :-
    \+ cache_check(superior(A, B, _, _)),
    superiorArgumentSupport(A, B, SupSet),
    cache_assert(superior(A, B, SupSet, true)), !.
superiorArgumentSupportBuffered(A, B, SupSet) :-
    \+ cache_check(superior(A, B, _, _)),
    cache_assert(superior(A, B, [], false)),
    fail, !.

superiorArgumentSupport([_, _, _, [LastDefRulesA, DefRulesA, DefPremisesA]], [_, _, _, [LastDefRulesB, DefRulesB, DefPremisesB]], SupSet) :-
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
	cache_check(preferences(Pref)),
	member(Rule, RulesA),
	allStronger(Pref, Rule, RulesB, SupSet), !.

weaker(RulesA, RulesB, SupSet) :-
	RulesA \== [],
	RulesB \== [],
	orderingComparator(democrat),
	cache_check(preferences(Pref)),
	weakerDemo(Pref, RulesA, RulesB, SupSet).

%(A, B) ∈ attnr(K) iff 1. A undercuts B, or 2. A rebuts B (at B′)
% and there is no defeasible rule d ∈ ldr(A) such that d ≺ last(B′).

weaker(RulesA, RulesB, [sup(X, W)]) :-
	RulesA \== [],
	RulesB \== [],
	orderingComparator(normal),
	cache_check(preferences(Pref)),
	member(W, RulesA),
	member(X, RulesB),
	member(sup(X, W), Pref), !.

weakerDemo(_, [], _, []).
weakerDemo(Pref, [H|T], Rules, [Sup|SupSet]) :-
	singleStronger(Pref, H, Rules, Sup),
	weakerDemo(Pref, T, Rules, SupSet).

singleStronger(Pref, Target, Rules, sup(Rule, Target)) :-
	member(Rule, Rules),
	member(sup(Rule, Target), Pref), !.

allStronger(_, _, [], []).
allStronger(Pref, Target, [Rule|Rules], [sup(Rule, Target)|SupSet]) :-
	member(sup(Rule, Target), Pref),
	allStronger(Pref, Target, Rules, SupSet).
