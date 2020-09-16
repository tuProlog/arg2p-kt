% ----------------------------------------------------------------
% statementLabelling.pl
% PIKA-lab
% Year: 2019
% ---------------------------------------------------------------


statementLabelling([IN, OUT, UND], [In, Ni, Und]) :-
    findall(Conc, member([_, _, Conc], IN), In),
    sort(In, SortIn),

    findall(Conc, member([_, _, Conc], OUT), PotentialNo),
    subtract(PotentialNo, In, Ni),
    sort(Ni, SortNi),

    findall(Conc, member([_, _, Conc], UND), PotentialUnd),
    subtract(PotentialUnd, In, Und),
    sort(Und, SortUnd),

    printStatementLabelling(  [SortIn, SortNi, SortUnd] ).