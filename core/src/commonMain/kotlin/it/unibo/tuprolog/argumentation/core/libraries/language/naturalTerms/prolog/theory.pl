%! clauses(+Rules, -Clauses, +StrictIds, -MappedIds)
%  The clauses of Rules, their statements written as text turned into terms; MappedIds are the strict/1 facts of
%  StrictIds for the rules as they are named then.
clauses(Rules, Clauses, StrictIds, MappedIds) :-
    ruleToClauseNatural(Rules, Clauses),
    mapIds(Rules, Clauses, StrictIds, MappedIds).

recoverId(Head :- Body, Id) :- parser::ruleClause(Id, _, _, Head, Body).
recoverId(Head :- Body, Id) :- parser::premiseClause(Id, _, Head, Body).
recoverId([Y, _, _], Y).
recoverId([Y, _], Y).

mapIds([], [], _, []).
mapIds([W|T], [Z|TT], Ids, [strict(Y)|RR]) :-
    once(recoverId(W, X)),
    member(strict(X), Ids), !,
    once(recoverId(Z, Y)),
    mapIds(T, TT, Ids, RR).
mapIds([_|T], [_|TT], Ids, RR) :-
    mapIds(T, TT, Ids, RR).
