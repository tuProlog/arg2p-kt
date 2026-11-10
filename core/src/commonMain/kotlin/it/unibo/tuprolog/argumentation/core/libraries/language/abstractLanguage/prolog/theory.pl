%! arguments(-Arguments)
%  The arguments the theory states or derives, in either notation (`a.`, arg(a)).
arguments(Arguments) :-
    findall(A, user::arg(A), Stated),
    findall(A, (parser::theoryClause(A, true), atom(A)), Atoms),
    append(Stated, Atoms, Arguments).

%! attacks(-Attacks)
%  The attacks the theory states or derives, in either notation (`a -> b.`, `a <-> b.`, att(a, b)), as att(A, B) terms.
attacks(Attacks) :-
    findall(att(A, B), user::att(A, B), Stated),
    findall(att(A, B), (parser::theoryClause((A -> B), Body), user::call(Body)), Forward),
    findall(att(A, B), (parser::theoryClause((X <-> Y), Body), user::call(Body), (A = X, B = Y ; A = Y, B = X)), Mutual),
    utils::appendLists([Stated, Forward, Mutual], Attacks).
