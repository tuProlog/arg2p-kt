% ----------------------------------------------------------------
% utilities.pl
% PIKA-lab
% Year: 2019
% -------------------------------------------------------------------------------


writeList([]).
writeList([X|Others]) :-
	write(X),write('\n'),
	writeList(Others).


%-----------------------------------------------------------------

assertaList([]).
assertaList([X|Others]) :-
	asserta(X),
	assertaList(Others).

%-----------------------------------------------------------------
sort(List,Sorted):-q_sort(List,[],Sorted).
q_sort([],Acc,Acc).
q_sort([H|T],Acc,Sorted):-
	pivoting(H,T,L1,L2),
	q_sort(L1,Acc,Sorted1),q_sort(L2,[H|Sorted1],Sorted).

pivoting(H,[],[],[]).
pivoting(H,[X|T],[X|L],G):- H @>= X, pivoting(H,T,L,G).
%X=<H,pivoting(H,T,L,G).
pivoting(H,[X|T],L,[X|G]):- X @> H, pivoting(H,T,L,G).
%X>H,pivoting(H,T,L,G).

%-----------------------------------------------------------------
subtract([], _, []).
subtract([Head|Tail], L2, L3) :-
        member(Head, L2), !,
        subtract(Tail, L2, L3).
subtract([Head|Tail1], L2, [Head|Tail3]) :-
        subtract(Tail1, L2, Tail3).

%------------------------------------------------------------------
isEmptyList([]).

%------------------------------------------------------------------
appendLists([], []).
appendLists([H|T], R) :-
    appendLists(T, AT),
    append(H, AT, R).

%------------------------------------------------------------------

search(F, L, S) :-
   between(1, L, N),
   functor(S, F, N),
   call(S).

between(N, M, K) :- N < M, K = N.
between(N, M, K) :- N == M, !, K = N.
between(N, M, K) :- N < M, N1 is N+1, between(N1, M, K).

%------------------------------------------------------------------

argumentChain(A, A, _) :- !.
argumentChain(A, B, Attacks) :-
    A \== B,
    member((_, A, C), Attacks),
    argumentChain(C, B, Attacks).

%------------------------------------------------------------------


%------------------------------------------------------------------------
% Superiority definition
% A superiority relation over a set of rules Rules is an antireflexive and
% antisymmetric binary relation over Rules
%------------------------------------------------------------------------

retractPreferenceCache :-
    retractall(superiorCache(_, _, _, _)).

superiorArgument(_, B, C) :- once(superiorArgumentSupportBuffered(C, B, _)).
superiorArgument(_, B, C, SupSet) :- once(superiorArgumentSupportBuffered(C, B, SupSet)).

superiorArgumentSupportBuffered(A, B, SupSet) :-
    superiorCache(A, B, SupSet, true).
superiorArgumentSupportBuffered(A, B, SupSet) :-
    superiorCache(A, B, SupSet, false),
    fail.
superiorArgumentSupportBuffered(A, B, SupSet) :-
    \+ superiorCache(A, B, SupSet, _),
    superiorArgumentSupport(A, B, SupSet),
    asserta(superiorCache(A, B, SupSet, true)).
superiorArgumentSupportBuffered(A, B, SupSet) :-
    \+ superiorCache(A, B, SupSet, _),
    asserta(superiorCache(A, B, SupSet, false)),
    fail.

superiorArgumentSupport(A, B, SupSet) :-
	argumentInfo(A, [LastDefRulesA, DefRulesA, DefPremisesA]),
	argumentInfo(B, [LastDefRulesB, DefRulesB, DefPremisesB]),
	superiorArgument(LastDefRulesA, DefRulesA, DefPremisesA, LastDefRulesB, DefRulesB, DefPremisesB, SupSet).

superiorArgument(LastDefRulesA, _, DefPremisesA, LastDefRulesB, _, DefPremisesB, SupSet) :-
    orderingPrinciple(last),
	superior(LastDefRulesA, DefPremisesA, LastDefRulesB, DefPremisesB, SupSet).

superiorArgument(_, DefRulesA, DefPremisesA, _, DefRulesB, DefPremisesB, SupSet) :-
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
	appendLists([SupSetA, SupSetB], SupSet).

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
	sup(X, W), !.

weakerDemo([], _, []).
weakerDemo([H|T], Rules, [Sup|SupSet]) :-
	singleStronger(H, Rules, Sup),
	weakerDemo(T, Rules, SupSet).

singleStronger(Target, Rules, sup(Rule, Target)) :-
	member(Rule, Rules),
	sup(Rule, Target), !.

allStronger(_, [], []).
allStronger(Target, [Rule|Rules], [sup(Rule, Target)|SupSet]) :-
	sup(Rule, Target),
	allStronger(Target, Rules, SupSet).
