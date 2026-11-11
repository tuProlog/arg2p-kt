%! rewrite(+Rules, +Ids, -Rewritten, -RewrittenIds)
%  The rules read (see parser::rewritten/4), their statements written as text turned into terms (see rulesInTerms/2);
%  RewrittenIds are the strict/1 facts of Ids for the rules as they are named then.
rewrite(Rules, Ids, Rewritten, RewrittenIds) :-
    rulesInTerms(Rules, Rewritten),
    mapIds(Rules, Rewritten, Ids, RewrittenIds).

mapIds([], [], _, []).
mapIds([[Id|_]|Rules], [[Named|_]|Rewritten], Ids, [strict(Named)|Mapped]) :-
    member(strict(Strict), Ids),
    Strict == Id, !,
    mapIds(Rules, Rewritten, Ids, Mapped).
mapIds([_|Rules], [_|Rewritten], Ids, Mapped) :-
    mapIds(Rules, Rewritten, Ids, Mapped).
