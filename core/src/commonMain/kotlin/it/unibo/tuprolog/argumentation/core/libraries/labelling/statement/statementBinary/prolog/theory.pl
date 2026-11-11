%! statementLabelling
%  The labels of the statements (see store::labelStatement/2): IN those with an argument IN, OUT the others
%  (their arguments are OUT or UND), each statement as it is written: one with variables and its instances are
%  labelled each on its own (see store::statementLabelled/2).
statementLabelling :-
    findall(_, (
        store::argumentLabel(in, A),
        store::statement(A, Conc),
        \+ store::statementLabelled(in, Conc),
        store::labelStatement(in, Conc)
    ), _),
    findall(_, (
        (store::argumentLabel(out, A) ; store::argumentLabel(und, A)),
        store::statement(A, Conc),
        \+ store::statementLabelled(in, Conc),
        \+ store::statementLabelled(out, Conc),
        store::labelStatement(out, Conc)
    ), _).
