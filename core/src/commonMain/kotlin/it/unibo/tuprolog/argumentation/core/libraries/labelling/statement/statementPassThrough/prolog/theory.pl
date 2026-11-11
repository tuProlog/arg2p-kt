%! statementLabelling
%  The labels of the statements (see store::labelStatement/2), one for each argument.
statementLabelling :-
    findall(_, (
        store::argumentLabel(Label, A),
        store::statement(A, Conc),
        store::labelStatement(Label, Conc)
    ), _).
