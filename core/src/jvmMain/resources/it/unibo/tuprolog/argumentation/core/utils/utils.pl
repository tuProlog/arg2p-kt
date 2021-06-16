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

superiorArgument(_, B, C) :- superiorArgument(C, B).

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
