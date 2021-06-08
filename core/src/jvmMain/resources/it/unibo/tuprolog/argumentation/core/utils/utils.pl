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

/*
*   Translates the attack relations identified during the building phase.
*   Attack from A to B -> Argument [[], attack, Attack]
*                         support(A, Argument)
*                         attack(Argument, B)
*   If an Argument A built in this way attacks the argument B, and this one also attacks a third argument C
*   through the argument B1 we have to consider an attack from A to B1 (transitive attack)
*/
convertAttack((T, A, B, C), [Arguments, Attacks, Supports], [[Id], attack, [attack(T, A, B)]], [Arguments, Attacks, Supports]) :-
    converted(T, A, B, C),
    argument([[Id], attack, [attack(T, A, B)]]).

convertAttack((T, A, B, C), [Arguments, Attacks, Supports], RArgument, [[RArgument|Arguments], ResAttacks, [(A, RArgument)|Supports]]) :-
    \+ converted(T, A, B, C),
    generateId(A, B, Id),
    RArgument = [[Id], attack, [attack(T, A, B)]],
    asserta(argument(RArgument)),
    asserta(support(A, RArgument)),
    asserta(attack(T, RArgument, B)),
    asserta(attack(T, RArgument, B, C)),
    retractall(attack(T, A, B)),
    retractall(attack(T, A, B, C)),
    transitiveConversion([(T, RArgument, B)|Attacks], [(A, RArgument)|Supports], ResAttacks),
    asserta(converted(T, A, B, C)), !.

transitiveConversion(Attacks, Supports, ResAttacks) :-
    transitiveConversionAttack(Attacks, Supports, (T, A, C, D)),
    asserta(attack(T, A, C)),
    asserta(attack(T, A, C, D)),
    transitiveConversion([(T, A, C)|Attacks], Supports, ResAttacks).
transitiveConversion(Attacks, _, Attacks).

transitiveConversionAttack(Attacks, Supports, (T, A, C, D)) :-
    member((T, A, B), Attacks),
    member((B, C), Supports),
    attackArgument(A),
    attackArgument(C),
    \+ attack(T, A, C),
    attack(T, A, B, D).

attackArgument([_, attack, _]).

generateId([IdA, _, _], [IdB, _, _], Res) :-
    concate(IdA, A),
    concate(IdB, B),
    concate([A, B], Res).

concate([],'').
concate([X|Tail], Res) :-
	concate(Tail, IntermediateRes),
   	atom_concat(X, IntermediateRes, Res).

