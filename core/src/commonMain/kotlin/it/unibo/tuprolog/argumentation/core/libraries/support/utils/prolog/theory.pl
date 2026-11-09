% Lists Utils

%! forEach(?Element, +List, +Goal)
%  Goal, all its solutions, for each element of List. Goal runs in this module: other modules are called through ::.
forEach(Element, List, Goal) :-
    findall(_, (member(Element, List), Goal), _).

%! cacheAll(+Clauses)
%  Each of Clauses asserted in the cache, in order.
cacheAll(Clauses) :-
    findall(_, (member(Clause, Clauses), cache_assert(Clause)), _).

%! oncePerTheory(+Key, +Goal)
%  What Goal reads of the theory, into the cache, read the first time it is asked for: the cache forgets it when the
%  theory changes (see Cache). The goal is solved in this module: it names the modules of its predicates.
oncePerTheory(Key, _) :- cache_check(read(Key)), !.
oncePerTheory(Key, Goal) :-
    Goal, !,
    cache_assert(read(Key)).

%! appendLists(+Lists, -Result)
%  Result is the concatenation of the lists in Lists.
appendLists([], []).
appendLists([List|Lists], Result) :-
    append(List, Rest, Result),
    appendLists(Lists, Rest).

% The others (contains/2, subtract/3, sortDistinct/2, deduplicate/2, sortDescending/2, intersectionOf/2, ...) are native primitives
