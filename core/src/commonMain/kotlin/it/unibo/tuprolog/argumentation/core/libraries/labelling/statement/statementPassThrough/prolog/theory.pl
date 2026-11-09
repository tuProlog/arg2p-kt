%! statementLabelling
%  The labels of the statements (see store::labelStatement/2), one for each argument.
statementLabelling :-
    findall(_, (
        store::conclusion(A, Conc),
        store::argumentLabel(Label, A),
        store::labelStatement(Label, Conc)
    ), _).
