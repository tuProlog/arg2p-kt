statementLabelling :-
    findall(_, (
        cache_dynamic_check(in([_, _, Conc, _, _])),
        \+ cache_dynamic_check(statIn(Conc)),
        cache_dynamic_assert(statIn(Conc))
    ), _),
    findall(_, (
        cache_dynamic_check(out([_, _, Conc, _, _])),
        \+ cache_dynamic_check(statIn(Conc)),
        \+ cache_dynamic_check(statOut(Conc)),
        cache_dynamic_assert(statOut(Conc))
    ), _),
    findall(_, (
        cache_dynamic_check(und([_, _, Conc, _, _])),
        \+ cache_dynamic_check(statIn(Conc)),
        \+ cache_dynamic_check(statOut(Conc)),
        \+ cache_dynamic_check(statUnd(Conc)),
        cache_dynamic_assert(statUnd(Conc))
    ), _).
