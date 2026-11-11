% Lists Utils

%! forEach(?Element, +List, +Goal)
%  Goal, all its solutions, for each element of List. Goal runs in this module: other modules are called through ::.
forEach(Element, List, Goal) :-
    findall(_, (member(Element, List), Goal), _).

%! cacheAll(+Clauses)
%  Each of Clauses asserted in the cache, in order.
cacheAll(Clauses) :-
    findall(_, (member(Clause, Clauses), cacheAssert(Clause)), _).

%! oncePerTheory(+Key, +Goal)
%  What Goal reads of the theory, into the cache, read the first time it is asked for: the cache forgets it when the
%  theory changes (see Cache). The goal is solved in this module: it names the modules of its predicates.
oncePerTheory(Key, _) :- cacheCheck(read(Key)), !.
oncePerTheory(Key, Goal) :-
    Goal, !,
    cacheAssert(read(Key)).

%! appendLists(+Lists, -Result)
%  Result is the concatenation of the lists in Lists.
appendLists([], []).
appendLists([List|Lists], Result) :-
    append(List, Rest, Result),
    appendLists(Lists, Rest).

%! tupleToList(+Conjunction, -List)
%  The items of a conjunction, as a list (built with an accumulator rather than appending the lists of the two sides).
tupleToList(T, L) :- tupleToList(T, L, []).
tupleToList(A, [A|R], R) :- var(A), !.
tupleToList((A, B), L, R) :- !, tupleToList(A, L, M), tupleToList(B, M, R).
tupleToList(A, [A|R], R).

% The others (contains/2, subtract/3, sortDistinct/2, deduplicate/2, sortDescending/2, intersectionOf/2, ...) are native primitives
