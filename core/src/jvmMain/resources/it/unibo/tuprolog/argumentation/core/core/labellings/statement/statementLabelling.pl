statementLabelling :-
    findall(_, (
        context_check(in([_, _, Conc, _, _])),
        \+ context_check(statIn(Conc)),
        context_assert(statIn(Conc))
    ), _),
    findall(_, (
        context_check(out([_, _, Conc, _, _])),
        \+ context_check(statIn(Conc)),
        \+ context_check(statOut(Conc)),
        context_assert(statOut(Conc))
    ), _),
    findall(_, (
        context_check(und([_, _, Conc, _, _])),
        \+ context_check(statIn(Conc)),
        \+ context_check(statOut(Conc)),
        \+ context_check(statUnd(Conc)),
        context_assert(statUnd(Conc))
    ), _).
