% Lists Utils

writeList([]) :- nl.
writeList([X|Others]) :-
	write(X),
	writeList(Others).


assertaList([]).
assertaList([X|Others]) :-
	asserta(X),
	assertaList(Others).


sortDistinct(List, Deduplicated) :-
    sort(List, Sorted),
    deduplicate(Sorted, Deduplicated).

deduplicate([], []).
deduplicate([H|T], Deduplicated) :-
    deduplicate(T, Deduplicated),
    member(H, Deduplicated), !.
deduplicate([H|T], [H|Deduplicated]) :-
    deduplicate(T, Deduplicated),
    \+ member(H, Deduplicated), !.


sort(List,Sorted) :- q_sort(List, [], Sorted).

q_sort([], Acc, Acc).
q_sort([H|T], Acc, Sorted) :-
	pivoting(H, T, L1, L2),
	q_sort(L1, Acc, Sorted1),
	q_sort(L2, [H|Sorted1], Sorted).

pivoting(H,[],[],[]).
pivoting(H, [X|T], [X|L], G) :-
    H @>= X,
    pivoting(H, T, L, G).
pivoting(H, [X|T], L, [X|G]) :-
    X @> H,
    pivoting(H, T, L, G).


subtract([], _, []).
subtract([Head|Tail], L2, L3) :-
        member(Head, L2), !,
        subtract(Tail, L2, L3).
subtract([Head|Tail1], L2, [Head|Tail3]) :-
        subtract(Tail1, L2, Tail3).


isEmptyList([]).


appendLists([], []).
appendLists([H|T], R) :-
    appendLists(T, AT),
    append(H, AT, R).

% Misc utils

store(Retract, Assert) :-
    cache_retract(Retract),
    cache_assert(Assert).


search(F, L, S) :-
   between(1, L, N),
   functor(S, F, N),
   call(S).

between(N, M, K) :- N < M, K = N.
between(N, M, K) :- N == M, !, K = N.
between(N, M, K) :- N < M, N1 is N+1, between(N1, M, K).
