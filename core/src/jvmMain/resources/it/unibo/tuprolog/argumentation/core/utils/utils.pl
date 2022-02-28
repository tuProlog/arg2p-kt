% Lists Utils

writeList([]) :- nl.
writeList([X|Others]) :-
	write(X),
	writeList(Others).

writeListNl([]).
writeListNl([X|Others]) :-
	write(X),nl,
	writeListNl(Others).


sortDistinct(List, Sorted) :-
    deduplicate(List, Deduplicated),
    sort(Deduplicated, Sorted).

deduplicate([], []).
deduplicate(List, Output) :-
    List \== [],
    setof(X, member(X, List), Output).

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
    append_fast(H, AT, R).

% Misc utils


search(F, L, S) :-
   between(1, L, N),
   functor(S, F, N),
   call(S).

between(N, M, K) :- N < M, K = N.
between(N, M, K) :- N == M, !, K = N.
between(N, M, K) :- N < M, N1 is N+1, between(N1, M, K).


recoverGraph(Args, Attacks, Supports) :-
        findall(X, context_check(argument(X)), TempArgs),
        findall((T, A, B, C), context_check(attack(T, A, B, C)), TempAttacks),
        findall((A, B), context_check(support(A, B)), TempSupports),
        utils::sort(TempArgs, Args),
        utils::sort(TempAttacks, Attacks),
        utils::sort(TempSupports, Supports).

recoverArgumentLabelling(ArgsIn, ArgsOut, ArgsUnd) :-
        findall(X, context_check(in(X)), TempArgsIn),
        findall(X, context_check(out(X)), TempArgsOut),
        findall(X, context_check(und(X)), TempArgsUnd),
        utils::sort(TempArgsIn, ArgsIn),
        utils::sort(TempArgsOut, ArgsOut),
        utils::sort(TempArgsUnd, ArgsUnd).

recoverStatementLabelling(In, Out, Und) :-
        findall(X, context_check(statIn(X)), TempIn),
        findall(X, context_check(statOut(X)), TempOut),
        findall(X, context_check(statUnd(X)), TempUnd),
        utils::sort(TempIn, In),
        utils::sort(TempOut, Out),
        utils::sort(TempUnd, Und).
