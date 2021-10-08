statementLabelling(SortedIn, SortedNo, SortedUnd) :-
    findall(Conc, cache_check(in([_, _, Conc, _, _])), In),
    findall(Conc, (
        cache_check(out([_, _, Conc, _, _])),
        \+ cache_check(in([_, _, Conc, _, _]))
    ), No),
    findall(Conc, (
        cache_check(und([_, _, Conc, _, _])),
        \+ cache_check(in([_, _, Conc, _, _])),
        \+ cache_check(out([_, _, Conc, _, _]))
    ), Und),
    utils::sort(In, SortedIn),
    utils::sort(No, SortedNo),
    utils::sort(Und, SortedUnd).
