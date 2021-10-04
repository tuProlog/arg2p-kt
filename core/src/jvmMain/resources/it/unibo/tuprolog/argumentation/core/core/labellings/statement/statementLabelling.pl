statementLabelling([ArgsIn, ArgsOut, ArgsUnd], [SortedIn, SortedNo, SortedUnd]) :-
    findall(Conc, member([_, _, Conc, _], ArgsIn), In),
    findall(Conc, (
        member([_, _, Conc, _], ArgsOut),
        \+ member(Conc, In)
    ), No),
    findall(Conc, (
        member([_, _, Conc, _], ArgsUnd),
        \+ member(Conc, In),
        \+ member(Conc, No)
    ), Und),
    sort(In, SortedIn),
    sort(No, SortedNo),
    sort(Und, SortedUnd).
