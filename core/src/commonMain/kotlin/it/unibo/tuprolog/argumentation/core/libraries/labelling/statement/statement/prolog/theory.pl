%! statementLabelling
%  The labels of the statements (see store::labelStatement/2): IN those with an argument IN, then OUT those
%  with an argument OUT, then UND those with an argument UND, each statement as it is written: one with variables
%  and its instances are labelled each on its own (see store::statementLabelled/2).
statementLabelling :-
    findall(_, (
        store::conclusion(A, Conc),
        store::argumentLabel(in, A),
        \+ store::statementLabelled(in, Conc),
        store::labelStatement(in, Conc)
    ), _),
    findall(_, (
        store::conclusion(A, Conc),
        store::argumentLabel(out, A),
        \+ store::statementLabelled(in, Conc),
        \+ store::statementLabelled(out, Conc),
        store::labelStatement(out, Conc)
    ), _),
    findall(_, (
        store::conclusion(A, Conc),
        store::argumentLabel(und, A),
        \+ store::statementLabelled(in, Conc),
        \+ store::statementLabelled(out, Conc),
        \+ store::statementLabelled(und, Conc),
        store::labelStatement(und, Conc)
    ), _).
